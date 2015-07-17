;;; nvm.el --- Manage Node versions within Emacs

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.3
;; Keywords: node, nvm
;; URL: http://github.com/rejeep/nvm.el
;; Package-Requires: ((s "1.8.0") (dash "2.4.0") (f "0.14.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'f)
(require 's)
(require 'dash)

(defgroup nvm nil
  "Manage Node versions within Emacs"
  :prefix "nvm-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/rejeep/nvm.el"))

(defconst nvm-version-re
  "v[0-9]+\.[0-9]+\.[0-9]+"
  "Regex matching a Node version.")

(defcustom nvm-dir (f-full "~/.nvm")
  "Full path to Nvm installation directory."
  :group 'nvm
  :type 'directory)

(defvar nvm-current-version nil
  "Current active version.")

(defun nvm--installed-versions ()
  (let ((match-fn (lambda (directory)
              (s-matches? (concat "^" nvm-version-re "$") (f-filename directory)))))
    (--map (f-filename it)
           (append
            (f-directories nvm-dir match-fn)
            (f-directories (f-join nvm-dir "versions" "node") match-fn)))))

(defun nvm--version-installed? (version)
  "Return true if VERSION is installed, false otherwise."
  (-contains? (nvm--installed-versions) version))

(defun nvm--find-exact-version-for (short)
  "Find most suitable version for SHORT.

SHORT is a string containing major and minor version.  This
function will return the most recent patch version."
  (when (s-matches? "^v?[0-9]+\.[0-9]+\\(\.[0-9]+\\)?$" short)
    (unless (s-starts-with? "v" short)
      (setq short (concat "v" short)))
    (let ((versions (nvm--installed-versions)))
      (if (--first (string= it short) versions)
          short
        (let ((possible-versions
               (-filter
                (lambda (version)
                  (s-starts-with? short version))
                versions)))
          (-min-by 'string< possible-versions))))))

(defun nvm-use (version &optional callback)
  "Activate Node VERSION.

If CALLBACK is specified, active in that scope and then reset to
previously used version."
  (setq version (nvm--find-exact-version-for version))
  (if (nvm--version-installed? version)
      (let ((prev-version nvm-current-version))
        (setenv "NVM_BIN" (f-join nvm-dir version "bin"))
        (setenv "NVM_PATH" (f-join nvm-dir version "lib" "node"))
        (let* ((path-re (concat "^" (f-join nvm-dir nvm-version-re "bin") "/?$"))
               (paths
                (cons
                 (f-full (f-join nvm-dir version "bin"))
                 (-reject
                  (lambda (path)
                    (s-matches? path-re path))
                  (parse-colon-path (getenv "PATH"))))))
          (setenv "PATH" (s-join path-separator paths)))
        (setq nvm-current-version version)
        (when callback
          (unwind-protect
              (funcall callback)
            (when prev-version (nvm-use prev-version)))))
    (error "No such version %s" version)))

(defun nvm-use-for (&optional path callback)
  "Activate Node for PATH or `default-directory'.

This function will look for a .nvmrc file in that path and
activate the version specified in that file.

If CALLBACK is specified, active in that scope and then reset to
previously used version."
  (unless path
    (setq path default-directory))
  (-if-let (nvmrc-path
            (f-traverse-upwards
             (lambda (dir)
               (f-file? (f-expand ".nvmrc" dir)))
             path))
      (nvm-use (s-trim (f-read (f-expand ".nvmrc" nvmrc-path))) callback)
    (error "No .nvmrc found for %s" path)))

(provide 'nvm)

;;; nvm.el ends here
