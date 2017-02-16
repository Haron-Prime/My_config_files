# coding: utf-8
from __future__ import unicode_literals, division, absolute_import, print_function

import sys
import threading

if sys.version_info < (3,):
    str_cls = unicode  # noqa
else:
    str_cls = str


__version__ = '1.0.1'
__version_info__ = (1, 0, 1)


_listeners = {}
_listener_lock = threading.Lock()


def listen(package_name, listener):
    """
    Adds a listener for a specific package

    :param package_name:
        A unicode string of the package to listen for events from

    :param listener:
        A callback that accepts three parameters:

         - a unicode string of the name of the package sending the event
         - a unicode string of the name of the event
         - an optional payload to send with the event, of any type

         THIS CALLBACK WILL BE RUN IN A THREAD OTHER THAN THE UI/MAIN THREAD.
    """

    if not isinstance(package_name, str_cls):
        raise TypeError('package_name must be a unicode string, not %s' % _type_name(package_name))

    _listener_lock.acquire()
    try:
        if package_name not in _listeners:
            _listeners[package_name] = set()
        _listeners[package_name].add(listener)
    finally:
        _listener_lock.release()


def notify(package_name, event_name, payload=None):
    """
    Sends a notification to all listeners for a package

    :param package_name:
        A unicode string of the name of the package sending the event

    :param event_name:
        A unicode string ot the name of the event

    :param payload:
        An optional payload to send with the event, preferrably an immutable
        object
    """

    if not isinstance(package_name, str_cls):
        raise TypeError('package_name must be a unicode string, not %s' % _type_name(package_name))

    if not isinstance(event_name, str_cls):
        raise TypeError('event_name must be a unicode string, not %s' % _type_name(event_name))

    if package_name not in _listeners:
        return

    def _do_notify():
        for listener in _listeners.get(package_name):
            listener(package_name, event_name, payload)
    threading.Thread(target=_do_notify).start()


def unlisten(package_name, listener):
    """
    Removes a listener for a specific package

    :param package_name:
        A unicode string of the package to unlisten to events from

    :param listener:
        The callback that was passed to listen()
    """

    if not isinstance(package_name, str_cls):
        raise TypeError('package_name must be a unicode string, not %s' % _type_name(package_name))

    _listener_lock.acquire()
    try:
        if package_name not in _listeners:
            return
        try:
            _listeners[package_name].remove(listener)
        except (KeyError):
            pass
    finally:
        _listener_lock.release()


def _type_name(value):
    """
    Returns a user-readable name for the type of an object

    :param value:
        A value to get the type name of

    :return:
        A unicode string of the object's type name
    """

    cls = value.__class__
    if cls.__module__ in set(['builtins', '__builtin__']):
        return cls.__name__
    return '%s.%s' % (cls.__module__, cls.__name__)
