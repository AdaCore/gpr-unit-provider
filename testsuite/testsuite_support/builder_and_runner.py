import os
from e3.os.process import PIPE, Run, STDOUT
from random import getrandbits
from e3.testsuite.driver.classic import TestAbortWithFailure

# environment variables definition
GUP_DIR = os.path.dirname (os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


class BuilderAndRunner(object):
    """
    This object provides wrappers for e3.os.process.Run, subprocess.call &
    subprocess.check_output
    """

    def __init__(self, driver=None):
        """configure BuilderAndRunner object using TestDriver environment or
        python script's argument when driver is None.
        """
        # associated TestDriver object
        self.driver = driver

    def simple_run(
        self,
        cmd,
        env=None,
        catch_error=True,
        output=PIPE,
        error=STDOUT,
        analyze_output=True,
    ):
        """ generic TestDriver.shell or e3.os.process.Run runner"""
        if self.driver is not None:
            if env is not None:
                effective_env = dict(os.environ)
                effective_env.update(env)
            return self.driver.shell(
                cmd,
                env=effective_env,
                catch_error=catch_error,
                analyze_output=analyze_output,
            )
        else:
            p = Run(cmd, env=env, output=output, error=error, ignore_environ=False)
            if catch_error and p.status != 0:
                print(str(cmd) + " returned " + str(p.status))
                print("stdout\n" + p.out)
                print("stderr\n" + p.err)
                raise TestAbortWithFailure("non-zero exit status")
            else:
                return p

    def build(self, project, vars=[], args=[], env=None, output=PIPE):
        """ gprbuild wrapper for normal & coverage modes """
        # If code coverage is requested, leave a chance to gnatcov to decorate
        # the execution of the subprogram in order to make it contribute to
        # code coverage.
        gprbuild_cmd = ["gprbuild", "-P", project] + vars + args

        return self.simple_run(gprbuild_cmd, env=env, output=output)

    def run(self, cmd, env=None, output=PIPE, catch_error=False):
        run_cmd = cmd

        return self.simple_run(run_cmd, env=env, catch_error=catch_error, output=output)

    def check_output(self, cmd):
        """subprocess.check_output wrapper handling coverage & valgrind
        modes.
        """
        return self.run(cmd, catch_error=True)

    def call(self, cmd):
        """subprocess.call wrapper handling coverage & valgrind
        modes.
        """
        p = self.run(cmd)
        print(p.out, end="")
        return p
