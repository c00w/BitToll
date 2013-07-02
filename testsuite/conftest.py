import pytest

def pytest_addoption(parser):
    parser.addoption("--server", action="store", default=None, dest="server")

