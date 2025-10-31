;;; firejail-mode.el --- Firejail profile mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Alexander Grafov
;;
;; Author: Alexander Grafov <grafov@inet.name>
;;
;; Keywords: languages, firejail, profiles
;; URL: https://github.com/grafov/firejail-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; Firejail mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; Firejail mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Firejail mode.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing Firejail profile files in GNU Emacs.

;;; Code:

(require 'smie)

(defvar firejail-mode-directives
  '("include" "blacklist" "whitelist" "noblacklist" "nowhitelist"
    "mkdir" "mkfile" "ignore")
  "Main directives used in `firejail-mode'.")

(defvar firejail-mode-options
  '("apparmor" "caps.drop" "caps.keep" "hostname" "ipc-namespace"
    "machine-id" "net" "netfilter" "no3d" "nodbus" "nodvd" "nogroups"
    "noinput" "nonewprivs" "noprinters" "noroot" "nosound" "notv"
    "nou2f" "novideo" "protocol" "seccomp" "seccomp.block-secondary"
    "seccomp.drop" "seccomp.error-action" "tracelog" "x11")
  "Options used in `firejail-mode'.")

(defvar firejail-mode-private-options
  '("disable-mnt" "private" "private-bin" "private-cache" "private-dev"
    "private-etc" "private-lib" "private-opt" "private-tmp")
  "Private options used in `firejail-mode'.")

(defvar firejail-mode-dbus-options
  '("dbus-user" "dbus-system" "dbus-user.filter" "dbus-user.own"
    "dbus-user.talk")
  "DBUS options used in `firejail-mode'.")

(defvar firejail-mode-special-options
  '("deterministic-shutdown" "env" "join-or-start"
    "memory-deny-write-execute" "noexec" "read-only" "read-write"
    "restrict-namespaces")
  "Special options used in `firejail-mode'.")

(defvar firejail-mode-landlock-options
  '("landlock.fs.read" "landlock.fs.write" "landlock.fs.makeipc"
    "landlock.fs.makedev" "landlock.fs.execute")
  "Landlock options used in `firejail-mode'.")

(defvar firejail-mode-allow-options
  '("allow-debuggers" "allusers" "keep-config-pulse" "keep-dev-ntsync"
    "keep-dev-shm" "keep-dev-tpm" "keep-fd" "keep-shell-rc" "keep-var-tmp"
    "writable-etc" "writable-run-user" "writable-var" "writable-var-log"
    "quiet" "warn")
  "Allow options used in `firejail-mode'.")

(defvar firejail-mode-keywords
  (append firejail-mode-directives
          firejail-mode-options
          firejail-mode-private-options
          firejail-mode-dbus-options
          firejail-mode-special-options
          firejail-mode-landlock-options
          firejail-mode-allow-options)
  "All keywords used in `firejail-mode'.")

(defconst firejail-mode-indent-offset 2
  "Indent firejail profile code by this number of spaces.")

(defvar firejail-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `firejail-mode'.")

(defvar firejail-mode-font-lock-defaults
  `((("#.*$" . font-lock-comment-face)
     (,(regexp-opt firejail-mode-keywords 'symbols) . font-lock-keyword-face)
     (,"\\${[A-Z_][A-Z0-9_]*}" . font-lock-constant-face)
     (,"/[^[:space:]\n]*" . font-lock-string-face)))
  "Font-lock defaults for `firejail-mode'.")

(defconst firejail-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; comments - # starts comment to end of line
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)

    ;; return our modified syntax table
    st))

(defun firejail-mode--backward-token ()
  "Same as `smie-default-backward-token'.
Expand this further on to get better parsing."
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (progn
     (if (zerop (skip-syntax-backward "."))
	 (skip-syntax-backward "w_'/"))
     (point))))

(defun firejail-mode--forward-token ()
  "Same as `smie-default-forward-token'.
Expand this further on to get better parsing."
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn
     (if (zerop (skip-syntax-forward "."))
	 (skip-syntax-forward "w_'/"))
     (point))))

(defconst firejail-mode--smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (toplevel (toplevel "\n" toplevel)))
     '((assoc "\n"))))))

(defun firejail-mode-smie-rules (kind token)
  "Rules for the smie grammar.

Argument KIND is one of \":elem\" \":before\" \":after\"
\":list-intro\", and is used to designate how to indent.

Argument TOKEN is the token in question, either defined by SMIE
or a custom made one."
  (pcase (cons kind token)
    (`(:elem . basic) firejail-mode-indent-offset)
    (`(:after . "\n") (smie-rule-separator kind))
    (`(:before . ,(or "include" "blacklist" "whitelist")) 0)))

;;;###autoload
(define-derived-mode firejail-mode prog-mode "Firejail"
  "Major mode for editing Firejail profile files."
  :syntax-table firejail-mode-syntax-table

  (setq-local tab-width firejail-mode-indent-offset)
  (setq-local indent-tabs-mode nil)
  (when (boundp 'electric-indent-inhibit) (setq electric-indent-inhibit t))

  (smie-setup firejail-mode--smie-grammar #'firejail-mode-smie-rules
	      :forward-token #'firejail-mode--forward-token
	      :backward-token #'firejail-mode--backward-token)

  (setq-local font-lock-defaults firejail-mode-font-lock-defaults)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (font-lock-ensure))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.profile\\'" . firejail-mode))

(provide 'firejail-mode)
;;; firejail-mode.el ends here

