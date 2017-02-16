# *pathtools* module for Package Control

This is a mirror of the [pathtools](https://github.com/gorakhargosh/pathtools) module, created for using it as *dependency* in [Package Control](http://packagecontrol.io) for [Sublime Text](http://sublimetext.com/).


this repo | pypi
---- | ----
![latest tag](https://img.shields.io/github/tag/vovkkk/pathtools.svg) | [![pypi](https://img.shields.io/pypi/v/pathtools.svg)](https://pypi.python.org/pypi/pathtools)


## How to use

1. Create a `dependencies.json` file in your package root with the following contents:

    ```js
    {
       "*": {
          "*": [
             "pathtools"
          ]
       }
    }
    ```

2. Run the **Package Control: Satisfy Dependencies** command via command palette

3. Import as you [usually do](http://pythonhosted.org/pathtools/api.html) in stand-alone Python, e.g.

    ```python
    from pathtools.path import absolute_path
    ```


See also:
[Documentation on Dependencies ](https://packagecontrol.io/docs/dependencies)

## License

[MIT](all/LICENSE)