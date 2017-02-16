# *watchdog* module for Package Control

This is a mirror of the [watchdog](https://github.com/gorakhargosh/watchdog) module, created for using it as *dependency* in [Package Control](http://packagecontrol.io) for [Sublime Text](http://sublimetext.com/).


this repo | pypi
---- | ----
![latest tag](https://img.shields.io/github/tag/vovkkk/sublime-watchdog.svg) | [![pypi](https://img.shields.io/pypi/v/watchdog.svg)](https://pypi.python.org/pypi/watchdog)


## How to use

1. Create a `dependencies.json` file in your package root with the following contents:

    ```js
    {
       "*": {
          "*": [
             "pathtools",
             "watchdog"
          ]
       }
    }
    ```

2. Run the **Package Control: Satisfy Dependencies** command via command palette

3. Import as you [usually do](http://pythonhosted.org/watchdog/quickstart.html) in stand-alone Python, e.g.

    ```python
    from watchdog.observers import Observer
    from watchdog.events import FileSystemEventHandler
    ```

4. Make sure to look at [*package_events*](https://github.com/codexns/package_events), if you’re wondering about emitting and listening events.

See also:
[Documentation on Dependencies ](https://packagecontrol.io/docs/dependencies)

## License

[Apache](all/COPYING)
