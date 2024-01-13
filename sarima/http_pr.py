import numpy as np
from flask import Flask, request, jsonify
import entry_point

app = Flask(__name__)


@app.route('/calculate', methods=['POST', 'GET'])
def calculate():
    print("calculate")
    data = request.get_json()  # получаем данные из тела запроса в формате JSON
    print("get data")
    print(data)
    print(np.array(data[1]).mean())
    result = entry_point.get_predictions(data[0], data[1])
    print("get result:")
    print(result)
    return jsonify(result.tolist())


if __name__ == '__main__':
    print("run")
    app.run(host='0.0.0.0', port=5000)
