import numpy as np

from sarima.sarima import Sarima

d = 1
D = 1
qs = range(1, 5)
Qs = range(1, 3)
ps = range(1, 5)
Ps = range(1, 3)
S = 12

from itertools import product
from tqdm import tqdm

parameters = product(ps, qs, Ps, Qs)
parameters_list = list(parameters)


def find_best_model(data):
    length = data.shape[0] - 1
    train_length = int(length * 0.8)

    results = []
    best_aic = float("inf")
    best_model = None
    best_param = None

    for param in tqdm(parameters_list):
        try:
            model = Sarima(data, order=(param[0], d, param[1]), seasonal_order=(param[2], D, param[3], S)).fit()
            model.predict_in_sample(raw=False)
        except ValueError as a:
            print('wrong parameters:', param)
            print(str(a))
            continue
        aic = model.get_aic()
        if aic < best_aic:
            best_model = model
            best_aic = aic
            best_param = param
        results.append([param, model.aic])
    return best_model, train_length, results, best_aic, best_param


def get_predictions(predict_len, data):
    model = find_best_model(np.array(data))[0]
    return model.forecast(steps=predict_len)
