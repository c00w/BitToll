from testlib import flushdb

def pytest_configure(config):
    flushdb()
