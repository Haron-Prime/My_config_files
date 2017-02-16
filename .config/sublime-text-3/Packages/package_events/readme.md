# package_events

This is a Package Control dependency that allows Sublime Text packages to
publish and listen for events from any package.

## Version

1.0.1 - [changelog](changelog.md)

## API

This dependency exposes three functions for package developers to utilize:

 - `package_events.listen()`
 - `package_events.notify()`
 - `package_events.unlisten()`

### `listen()`

The `listen()` function requires two parameters:

 - a unicode string of the package name to listen to events for
 - a callback to call when an event is published, accepting three paramters:
   - a unicode string of the package publishing the event
   - a unicode string of the name of the event
   - a message payload, the type of which is set by the publishing package 

**The callback will always be called in a thread other than the UI/main
thread.** Thus, if interaction with the Sublime Text API is necessary by the
event handler, `sublime.set_timeout()` should be called to schedule code to be
run in the UI thread.

```python
import package_events

def event_handler(package, name, payload):
    print('%s just sent the event %s with a payload of %s' % (package, name, repr(payload)))

package_events.listen('Other Package', event_handler)
```

### `notify()`

The function `notify()` is used to send out an event. Every callback registered
by `listen()` will be called, passing the package name, event name and payload.

The function requires three parameters:

 - a unicode string of the name of the package sending the event
 - a unicode string of the name of the event
 - an optional payload to send with the event, preferrably an immutable object

*While any type of value can be sent as a payload, it is highly recommended to
send an immutable value to prevent accidental modification of the payload
by listeners. For a `dict`-like option, look into `collections.namedtuple`.*

```python
import package_events
import subprocess

proc = subprocess.Popen(['command', '-arg'])
process_id = proc.pid

package_events.notify('My Package', 'process_started', process_id)

proc.wait()

package_events.notify('My Package', 'process_completed', process_id)
```

### `unlisten()`

The `unlisten()` function can be used to removed a listener from the list of
listeners. It accepts two parameters:

 - a unicode string of the package name to remove the listener for
 - the callback function to remove

```python
import package_events

def event_handler(package, name, payload):
    print('%s just sent the event %s with a payload of %s' % (package, name, repr(payload)))
    package_events.unlisten('Other Package', event_handler)

package_events.listen('Other Package', event_handler)
```

## Requiring

To have a package require `package_events`, add a file named `dependencies.json`
into the root folder of your Sublime Text package, and add the following:

```json
{
    "*": {
        "*": [
            "package_events"
        ]
    }
}
```

This indicates that for all operating systems (`*`), and all versions of
Sublime Text (nested `*`), require the `package_events` dependency. You can
also read the
[official documentation about dependencies](https://packagecontrol.io/docs/dependencies).
