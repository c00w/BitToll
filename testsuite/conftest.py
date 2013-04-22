from testlib import flushdb

def pytest_configure(config):
    print 'configure called'
    flushdb()
