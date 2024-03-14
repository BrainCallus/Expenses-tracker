FROM python:3

ADD . /sarima
WORKDIR /sarima
RUN pip install --upgrade pip; pip install -U setuptools;pip install -r /sarima/requirements.txt --upgrade

EXPOSE 5000


CMD ["python", "http_pr.py", "--debug"]