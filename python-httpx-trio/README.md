Easy Racer Python HTTPX
-----------------------

```
git clone https://github.com/pyenv/pyenv.git .pyenv
export PYENV_ROOT=.pyenv
.pyenv/bin/pyenv install
eval "$(.pyenv/bin/pyenv init --path)"
python -m venv venv
source venv/bin/activate
pip install -U pip
pip install -r requirements.txt
```

Run against local server:
```
python main.py
```

Test with EasyRacer Container:
```
pytest
```
