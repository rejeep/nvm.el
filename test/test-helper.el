(require 'f)

(defvar nvm-test/test-path
  (f-parent (f-this-file)))

(defvar nvm-test/root-path
  (f-parent nvm-test/test-path))

(require 'ert)
(require 'el-mock)
(require 'nvm (f-expand "nvm" nvm-test/root-path))
