from abc import abstractmethod

import numpy as np
import statsmodels.api as sm
from statsmodels.tools.validation.validation import array_like
from statsmodels.tsa.arima.params import SARIMAXParams
from statsmodels.tsa.arima.specification import SARIMAXSpecification


class Sarimax:
    def __init__(self, data, order=(1, 0, 0), seasonal_order=(0, 0, 0, 0)):
        self.data = self.prepare_data_array(data)

        self.data_cpy = self.data.copy()

        self.order = order
        self.seasonal_order = seasonal_order

        self.spec = SARIMAXSpecification(self.data_cpy, None, self.order, self.seasonal_order)
        self.params = SARIMAXParams(self.spec)

        self.p, self.d, self.q = self.order
        self.P, self.D, self.Q, self.seasonal_period = self.seasonal_order

    @abstractmethod
    def fit(self, *args, **kwargs):
        pass

    @abstractmethod
    def predict_in_sample(self, start=None, end=None):
        pass

    @abstractmethod
    def forecast(self, steps=1, method=None):
        pass

    @abstractmethod
    def _set_params(self, *args):
        pass

    def integrate_predictions(self, predictions):
        preds_size = predictions.size

        if self.d != 0:
            predictions += sum(
                self.shift(data=self.simple_diff(self.data_cpy, i), crop=True)[-preds_size:] for i in range(self.d))

        if self.D != 0:
            data_diff = self.simple_diff(self.data_cpy, self.d)
            predictions += sum(
                self.shift(data=self.seasonal_diff(data_diff, i, self.seasonal_period),
                           periods=self.seasonal_period, crop=True)[-preds_size:] for i in range(self.D)
            )
        return predictions

    @staticmethod
    def simple_diff(data, d=1):
        return np.diff(data, n=d, axis=0)

    @staticmethod
    def seasonal_diff(data, D, periods):
        return sm.tsa.statespace.tools.diff(data, k_diff=0, k_seasonal_diff=D, seasonal_periods=periods)

    @staticmethod
    def shift(data, periods=1, crop=False):
        data = np.atleast_1d(np.asanyarray(data))
        fill_val = np.nan
        numel = data.size
        periods = periods % numel

        filler = np.full((periods,), fill_val, dtype=data.dtype)
        shifted = np.r_[filler, data[:numel - periods]]

        if crop:
            shifted = shifted[periods:]
        return shifted

    @staticmethod
    def prepare_data_array(data):
        if data is None:
            return None
        data_ = np.asanyarray(data)
        data_ = np.squeeze(data_)
        data_ = array_like(data_, "data", ndim=1)
        return data_

    @staticmethod
    def empty_array():
        return np.array([])
