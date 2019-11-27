;;; build-farm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "build-farm" "build-farm.el" (0 0 0 0))
;;; Generated autoloads from build-farm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "build-farm" '("build-farm-")))

;;;***

;;;### (autoloads nil "build-farm-build" "build-farm-build.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from build-farm-build.el

(autoload 'build-farm-latest-builds "build-farm-build" "\
Display latest NUMBER of builds.
ARGS are the same arguments as for `build-farm-build-latest-api-url'.
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it and for the other
ARGS.

\(fn NUMBER &rest ARGS)" t nil)

(autoload 'build-farm-queued-builds "build-farm-build" "\
Display the NUMBER of queued builds.
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it.

\(fn NUMBER)" t nil)

(autoload 'build-farm-build "build-farm-build" "\
Find build by its ID and display it.

\(fn ID)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "build-farm-build" '("build-farm-")))

;;;***

;;;### (autoloads nil "build-farm-evaluation" "build-farm-evaluation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from build-farm-evaluation.el

(autoload 'build-farm-latest-evaluations "build-farm-evaluation" "\
Display latest NUMBER of evaluations.
Interactively, use `build-farm-number-of-builds' variable for
NUMBER.  With prefix argument, prompt for it.

\(fn NUMBER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "build-farm-evaluation" '("build-farm-")))

;;;***

;;;### (autoloads nil "build-farm-jobset" "build-farm-jobset.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from build-farm-jobset.el

(autoload 'build-farm-jobsets "build-farm-jobset" "\
Display jobsets of PROJECT.
PROJECT is required for Hydra build farm and is not needed for
Cuirass.

\(fn &optional PROJECT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "build-farm-jobset" '("build-farm-")))

;;;***

;;;### (autoloads nil "build-farm-popup" "build-farm-popup.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from build-farm-popup.el
 (autoload 'build-farm-popup "build-farm-popup" nil t)

(defalias 'build-farm #'build-farm-popup "\
Popup interface for the available build farm commands.")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "build-farm-popup" '("build-farm-")))

;;;***

;;;### (autoloads nil "build-farm-project" "build-farm-project.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from build-farm-project.el

(autoload 'build-farm-projects "build-farm-project" "\
Display build farm projects." t nil)

(autoload 'build-farm-project "build-farm-project" "\
Display build farm PROJECT.

\(fn PROJECT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "build-farm-project" '("build-farm-project-")))

;;;***

;;;### (autoloads nil "build-farm-url" "build-farm-url.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from build-farm-url.el

(autoload 'build-farm-set-url "build-farm-url" "\
Set variable `build-farm-url' to URL.
Interactively, prompt for URL.

\(fn URL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "build-farm-url" '("build-farm-")))

;;;***

;;;### (autoloads nil "build-farm-utils" "build-farm-utils.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from build-farm-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "build-farm-utils" '("build-farm-")))

;;;***

;;;### (autoloads nil nil ("build-farm-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; build-farm-autoloads.el ends here
