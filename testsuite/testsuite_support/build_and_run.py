import os
import os.path

from e3.testsuite.driver.classic import TestAbortWithError
from e3.env import Env

from testsuite_support.base_driver import BaseDriver
from testsuite_support.builder_and_runner import BuilderAndRunner


class BuildAndRunDriver(BaseDriver):
    """
    Driver to build a test program and run it.

    Interface:

    * put a project file for a program and the associated source files in the
      test directory;
    * put a "test.out" text file in the test directory;
    * in the "test.yaml" file, add a "project_file" key that contains the name
      of the project file, and add also a "main" key that contains the name of
      the program to run.

    This driver will build the program using GPRbuild and will then run the
    program. The output of this run is checked against the expected output
    (test.out file).
    """

    def set_up(self):
        super(BuildAndRunDriver, self).set_up()

        project_file = self.test_env.get("project_file", None)
        main = self.test_env.get("main", None)

        if not project_file or not isinstance(project_file, str):
            raise TestAbortWithError(
                'test.yaml: please define a "project_file" string field'
            )
        if not main or not isinstance(main, str):
            raise TestAbortWithError('test.yaml: please define a "main" string field')

        self.project_file = project_file
        self.main_program = main

        self.builder_and_runner = BuilderAndRunner(self)

    def run(self):
        env = {}

        self.builder_and_runner.build(
            project=self.project_file,
            args=["-g1", "-q", "-p", "-bargs", "-Es"],
            env=env,
        )

        self.builder_and_runner.run([os.path.join(".", self.main_program)], env=env)
