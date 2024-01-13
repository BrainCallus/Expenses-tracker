from collections import deque
from itertools import chain

import numpy as np
from sklearn.linear_model import SGDRegressor

from statsmodels.tsa.arima.estimators.hannan_rissanen import hannan_rissanen

from statsmodels.tsa.tsatools import lagmat

from sarima import sarimax


class Sarima(sarimax.Sarimax):
    def __init__(self, data, order=(1, 0, 0), seasonal_order=(0, 0, 0, 0)):
        super(Sarima, self).__init__(data, order, seasonal_order)

        self.data = (self.data - self.data.mean()) / self.data.std()

        self.init_data_mean = self.data_cpy.mean()
        self.init_data_std = self.data_cpy.std()
        self.data_cpy = self.data.copy()

        self.regressor = None
        self.design_matrix = None

        self.preds_in_sample = None
        self.raw_preds_in_sample = None
        self.residuals = None
        self.aic = None

    def fit(self):
        if self.design_matrix is None:
            self.design_matrix = self.create_design_matrix()

        self.regressor = SGDRegressor(loss="squared_error", penalty="l2", alpha=0.0001, l1_ratio=0.15,
                                      fit_intercept=False, max_iter=20_000, tol=None, shuffle=False,
                                      verbose=0, epsilon=0.1, random_state=None,
                                      learning_rate="invscaling", eta0=0.01, power_t=0.25,
                                      early_stopping=False, validation_fraction=0.1,
                                      n_iter_no_change=5, warm_start=False, average=False)

        self.regressor = self.regressor.fit(self.design_matrix, self.data)

        self._set_params()
        return self

    def predict_in_sample(self, start=0, end=None, raw=False):

        if self.preds_in_sample is not None:
            return self.raw_preds_in_sample[start:end] if raw else self.preds_in_sample[start:end]

        predictions = self.regressor.predict(self.design_matrix)
        if self.raw_preds_in_sample is None:
            self.raw_preds_in_sample = predictions.copy()

        self.residuals = self.data - self.raw_preds_in_sample

        if raw:
            return predictions[start:end]

        predictionss = self.integrate_predictions(predictions) * self.init_data_std + self.init_data_mean

        self.preds_in_sample = predictionss

        return predictionss[start:end]

    def forecast(self, steps=1, method=None):
        p, d, q = self.order
        P, D, Q, S = self.seasonal_order

        ar_coeffs_cpy = -self.params.reduced_ar_poly.coef[1:]
        ar_size_cpy = ar_coeffs_cpy.size
        ar_coeffs = np.r_[ar_coeffs_cpy, np.zeros((S * P + p) - ar_size_cpy)]

        ma_coeffs_cpy = self.params.reduced_ma_poly.coef
        ma_size_cpy = ma_coeffs_cpy.size
        ma_coeffs = np.r_[ma_coeffs_cpy, np.zeros((S * Q + q + 1) - ma_size_cpy)]

        last_obs = deque(self.data[-1: - (S * P + p + 1):-1], maxlen=S * P + p)

        if self.residuals is None:
            self.predict_in_sample(raw=True)

        last_resids = deque(np.r_[0, self.residuals[-1:(S * Q + q + 1):-1]], maxlen=S * Q + q + 1)

        forecasts = np.empty(steps)
        xtended_data = self.data_cpy[-(S * D + d):].tolist()
        for i in range(steps):
            forecasts[i] = ar_coeffs.dot(last_obs) + ma_coeffs.dot(last_resids)

            for j in range(d):
                forecasts[i] += self.simple_diff(xtended_data, j)[-1]

            data_diff = self.simple_diff(xtended_data, d)

            for j in range(D):
                forecasts[i] += self.seasonal_diff(data_diff, j, S)[-S]

            last_obs.appendleft(forecasts[i])
            last_resids.appendleft(0)

        forecasts = forecasts * self.init_data_std + self.init_data_mean

        return forecasts

    def _set_params(self):
        params = self.params
        params.ar_params = self.get_ar_params()
        params.ma_params = self.get_ma_params()
        params.seasonal_ar_params = self.get_seasonal_ar_params()
        params.seasonal_ma_params = self.get_seasonal_ma_params()

    def get_aic(self):
        if self.aic is None:
            if self.residuals is None:
                raise RuntimeError('Can\'t compute aic before predictions made')
            data_len = len(self.data)
            self.aic = data_len * np.log(sum(map(lambda x: x ** 2, self.residuals)) / data_len) + 2 * 8
        return self.aic

    def get_ar_params(self):
        p = self.order[0]
        return self.regressor.coef_[:p] if self.regressor is not None and p > 0 else self.empty_array()

    def get_ma_params(self):
        if self.regressor is not None and self.q > 0:
            lower = self.p + self.P + self.p * self.P
            return self.regressor.coef_[lower:lower + self.q]
        else:
            return self.empty_array()

    def get_seasonal_ar_params(self):
        if self.regressor is not None and self.P > 0:
            lower = self.p
            return self.regressor.coef_[lower:lower + self.P]
        else:
            return self.empty_array()

    def get_seasonal_ma_params(self):
        if self.regressor is not None and self.Q > 0:
            lower = self.p + self.P + self.p * self.P + self.q
            return self.regressor.coef_[lower:lower + self.Q]
        else:
            return self.empty_array()

    def create_design_matrix(self):
        if self.d != 0:
            self.data = self.simple_diff(self.data, d=self.d)

        if self.D != 0:
            self.data = self.seasonal_diff(self.data, D=self.D, periods=self.seasonal_period)

        data_size = self.data.size

        if self.q > 0 or self.Q > 0:
            reduced_ar_poly = list(chain.from_iterable([[1] * self.p,
                                                        *[[0] * (self.seasonal_period - self.p - 1) + [1] * (self.p + 1)
                                                          for _ in range(self.P)]]))

            reduced_ma_poly = list(chain.from_iterable([[1] * self.q,
                                                        *[[0] * (self.seasonal_period - self.q - 1) + [1] * (self.q + 1)
                                                          for _ in range(self.Q)]]))

            _, hr_results = hannan_rissanen(self.data, ar_order=reduced_ar_poly, ma_order=reduced_ma_poly,
                                            demean=False)
            hr_residuals = hr_results.resid
            residual_size = hr_residuals.size
            self.resid = np.r_[np.zeros(data_size - residual_size), hr_residuals]

        empty_part = np.array([]).reshape(data_size, 0)
        p_part = (self.get_non_seasonal_part(self.data, self.p) if self.p > 0 else empty_part)

        P_part = (self.get_seasonal_part(self.data, self.P, self.seasonal_period) if self.P > 0 else empty_part)

        p_mixed_part = (self.get_mixed_part(P_part, self.p, self.P) if self.p * self.P > 0 else empty_part)

        q_part = (self.get_non_seasonal_part(self.resid, self.q) if self.q > 0 else empty_part)

        Q_part = (self.get_seasonal_part(self.resid, self.Q, self.seasonal_period) if self.Q > 0 else empty_part)

        q_mixed_part = (self.get_mixed_part(Q_part, self.q, self.Q) if self.q * self.Q > 0 else empty_part)

        return np.hstack((p_part, P_part, p_mixed_part, q_part, Q_part, q_mixed_part, empty_part, empty_part))

    @staticmethod
    def get_non_seasonal_part(data_or_residuals, order):
        return lagmat(data_or_residuals, maxlag=order)

    @staticmethod
    def get_seasonal_part(data_or_residuals, order, seasons):
        data_size = data_or_residuals.shape[0]
        part = np.empty((data_size, order))
        for i in range(order):
            part[:, i] = np.r_[np.zeros((i + 1) * seasons), data_or_residuals[: data_size - (i + 1) * seasons]]
        return part

    @staticmethod
    def get_mixed_part(seasonal_part, order, seasonal_order):
        seas_sz = seasonal_part.shape[0]
        part = np.empty((seas_sz, order * seasonal_order))
        for i in range(seasonal_order):
            part[:, i * order: (i + 1) * order] = lagmat(seasonal_part[:, i], maxlag=order)
        return part
