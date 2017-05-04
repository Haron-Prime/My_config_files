from lint import Linter

class Lua(Linter):
    language = ('coronasdklua', 'lua')
    cmd = ('luac', '-p')
    regex = '^luac: [^:]+:(?P<line>\d+): (?P<error>.+)'

    def run(self, cmd, code):
        return self.tmpfile(cmd, code, suffix='.lua')
