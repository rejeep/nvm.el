(eval-when-compile
  (defvar nvm-dir))

(defmacro with-sandbox (&rest body)
  `(with-mock
    (let ((nvm-dir "/path/to/nvm/")
          (process-environment
           '("NVM_BIN=/path/to/nvm/v0.0.1/bin"
             "NVM_PATH=/path/to/nvm/v0.0.1/lib/node"
             "PATH=/path/to/foo/bin/:/path/to/nvm/v0.0.1/bin/:/path/to/bar/bin/")))
      ,@body)))

(defun should-have-env (env value)
  (should (string= (getenv env) value)))

(defun should-use-version (version)
  (should-have-env "NVM_BIN" (f-join nvm-dir version "bin"))
  (should-have-env "NVM_PATH" (f-join nvm-dir version "lib" "node"))
  (should-have-env "PATH" (concat (f-full (f-join nvm-dir version "bin")) ":/path/to/foo/bin/:/path/to/bar/bin/")))

(ert-deftest nvm-use-test/version-not-available ()
  (should-error
   (nvm-use "v0.10.1")))

(ert-deftest nvm-use-test/version-available-no-callback ()
  (with-sandbox
   (stub nvm--installed-versions => '("v0.10.1"))
   (nvm-use "v0.10.1")
   (should-use-version "v0.10.1")))

(ert-deftest nvm-use-test/version-available-with-callback ()
  (with-sandbox
   (stub nvm--installed-versions => '("v0.8.2" "v0.10.1"))
   (nvm-use "v0.8.2")
   (should-use-version "v0.8.2")
   (nvm-use "v0.10.1"
            (lambda ()
              (should-use-version "v0.10.1")))
   (should-use-version "v0.8.2")))

(ert-deftest nvm-use-test/version-available-with-callback-that-errors ()
  (with-sandbox
   (stub nvm--installed-versions => '("v0.8.2" "v0.10.1"))
   (nvm-use "v0.8.2")
   (should-use-version "v0.8.2")
   (should-error
    (nvm-use "v0.10.1" (lambda () (error "BooM"))))
   (should-use-version "v0.8.2")))
