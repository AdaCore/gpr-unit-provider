import os
import os.path

from e3.env import Env
from e3.fs import mkdir
from e3.testsuite.driver.classic import TestAbortWithError
from e3.testsuite.driver.diff import DiffTestDriver, ReplacePath, Substitute


class BaseDriver(DiffTestDriver):
    """Base class to provide common test driver helpers."""

    def set_up(self):
        super(BaseDriver, self).set_up()

        try:
            self.test_env['description']
        except KeyError:
            raise TestAbortWithError(
                'test.yaml: missing "description" field'
            )

    @property
    def output_refiners(self):
        # Remove working directory from output and
        # make all filenames look like Unix ones (forward slashes for directory
        # separators).
        return super().output_refiners + [
            ReplacePath(self.working_dir(), replacement=""),
            Substitute('\\', '/')
        ]

    # Convenience path builders

    @property
    def testsuite_dir(self):
        """Return the absolute path to the testsuite root directory."""
        result = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              '..')
        return os.path.abspath(result)
