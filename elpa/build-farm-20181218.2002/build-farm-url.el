;;; build-farm-url.el --- Build farm URLs  -*- lexical-binding: t -*-

;; Copyright © 2015–2018 Alex Kost <alezost@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides the code to determine various URLs of the build
;; farms and to receive data from them.

;;; Code:

(require 'url-handlers)
(require 'url-expand)
(require 'json)
(require 'build-farm-utils)
(eval-when-compile (require 'subr-x))

(defvar build-farm-url-alist
  '(("https://hydra.nixos.org" . hydra)
    ("https://hydra.gnu.org" . hydra)
    ("https://ci.guix.info" . cuirass)
    ("https://berlin.guixsd.org" . cuirass))
  "Alist of URLs and their types of the available build farms.")

(defun build-farm-guess-url ()
  "Return URL of a build farm that a user probably wants to use."
  (if (eq 'guix build-farm-preferred-package-manager)
      "https://ci.guix.info"
    "https://hydra.nixos.org"))

(defun build-farm-urls ()
  "Return a list of available build farm URLs."
  (mapcar #'car build-farm-url-alist))

(defcustom build-farm-url (build-farm-guess-url)
  "URL of the default build farm."
  :type `(choice ,@(mapcar (lambda (url) (list 'const url))
                           (build-farm-urls))
                 (string :tag "Other URL"))
  :group 'build-farm)

(defun build-farm-read-url ()
  "Read from minibuffer and return build farm URL."
  (completing-read "Build farm URL: "
                   (build-farm-urls)
                   nil nil nil nil
                   build-farm-url))

;;;###autoload
(defun build-farm-set-url (url)
  "Set variable `build-farm-url' to URL.
Interactively, prompt for URL."
  (interactive (list (build-farm-read-url)))
  (setq build-farm-url url))

(defun build-farm-url-type (&optional url)
  "Return build farm type by its URL.
If URL is nil, use variable `build-farm-url'."
  (or (bui-assoc-value build-farm-url-alist
                       (or url build-farm-url))
      (let ((type (if (string-match-p "cuirass" url)
                      'cuirass
                    'hydra)))
        (message "Unknown URL: <%s>.
Consider adding it to `build-farm-url-alist'.
Arbitrarily choosing `%S' type for this URL."
                 url type)
        type)))

(defun build-farm-url-package-manager (&optional url)
  "Return a package manager for the build farm URL.
The returned value is either `nix' or `guix' symbols or nil, if
the package manager cannot be determined.
If URL is nil, use variable `build-farm-url'."
  (or url (setq url build-farm-url))
  (cond ((or (string-match-p (regexp-opt '("gnu" "guix")) url)
             (eq 'cuirass (build-farm-url-type url)))
         'guix)
        ((string-match-p "nix" url)
         'nix)))

(defun build-farm-url (&optional root-url &rest url-parts)
  "Return build farm ROOT-URL with URL-PARTS concatenated to it.
If ROOT-URL is nil, use variable `build-farm-url'."
  (url-expand-file-name (mapconcat #'identity url-parts "")
                        (or root-url build-farm-url)))

(cl-defun build-farm-api-url (type args &key root-url)
  "Return URL for receiving data using build farm API.
See function `build-farm-url' for the meaning of ROOT-URL.
TYPE is the name of an allowed method.
ARGS is alist of (KEY . VALUE) pairs.
Skip ARG, if VALUE is nil or an empty string."
  (let* ((fields (mapcar
                  (lambda (arg)
                    (pcase arg
                      (`(,key . ,value)
                       (unless (or (null value)
                                   (equal "" value))
                         (concat (build-farm-hexify key) "="
                                 (build-farm-hexify value))))
                      (_ (error "Wrong argument '%s'" arg))))
                  args))
         (fields (mapconcat #'identity (delq nil fields) "&")))
    (build-farm-url root-url "api/" type "?" fields)))

(cl-defun build-farm-build-url (id &key root-url)
  "Return URL of a build ID.
See function `build-farm-url' for the meaning of ROOT-URL."
  (build-farm-url root-url "build/" (number-to-string id)))

(cl-defun build-farm-build-log-url (id &key root-url raw)
  "Return URL of the build log of a build ID.
If RAW is non-nil, return url of the raw build log file.
See function `build-farm-url' for the meaning of ROOT-URL."
  (concat (build-farm-build-url id :root-url root-url)
          "/log"
          (if raw "/raw" "")))

(cl-defun build-farm-build-latest-api-url
    (number &key root-url project jobset job system)
  "Return API URL to receive latest NUMBER of builds.
See function `build-farm-url' for the meaning of ROOT-URL."
  (build-farm-api-url
   "latestbuilds"
   `(("nr" . ,number)
     ("project" . ,project)
     ("jobset" . ,jobset)
     ("job" . ,job)
     ("system" . ,system))
   :root-url root-url))

(cl-defun build-farm-build-queue-api-url (number &key root-url)
  "Return API URL to receive the NUMBER of queued builds.
See function `build-farm-url' for the meaning of ROOT-URL."
  (build-farm-api-url
   "queue"
   `(("nr" . ,number))
   :root-url root-url))

(cl-defun build-farm-jobset-url (&key root-url project jobset jobset-id)
  "Return URL of a build farm JOBSET.

For Cuirass farm, you should not use PROJECT, so you can specify
either JOBSET or JOBSET-ID.

For Hydra farm, you should specify either a single JOBSET-ID
argument (it should have a form 'project/jobset') or PROJECT and
JOBSET arguments.

See function `build-farm-url' for the meaning of ROOT-URL."
  (build-farm-url root-url "/jobset/"
                  (if project
                      (concat project "/" jobset)
                    ;; JOBSET-ID for Cuirass contains leading "/".
                    (or (string-trim-left jobset-id "/")
                        jobset))))

(cl-defun build-farm-hydra-jobset-api-url (project &key root-url)
  "Return API URL for Hydra jobsets by PROJECT.
See function `build-farm-url' for the meaning of ROOT-URL."
  (build-farm-api-url
   "jobsets"
   `(("project" . ,project))
   :root-url root-url))

(cl-defun build-farm-cuirass-jobsets-url (&key root-url)
  "Return URL with all Cuirass JOBSETS.
See function `build-farm-url' for the meaning of ROOT-URL."
  (build-farm-url root-url "/jobsets"))

(cl-defun build-farm-project-url (&key root-url project)
  "Return URL with build farm PROJECT.
If PROJECT is nil, return URL with all projects.
See function `build-farm-url' for the meaning of ROOT-URL."
  (if project
      (build-farm-url root-url "project/" project)
    (build-farm-url root-url)))

(cl-defun build-farm-evaluation-url (&key root-url evaluation)
  "Return URL with build farm EVALUATION (number or string).
See function `build-farm-url' for the meaning of ROOT-URL."
  (build-farm-url root-url "eval/"
                  (if (stringp evaluation)
                      evaluation
                    (number-to-string evaluation))))

(cl-defun build-farm-latest-evaluations-api-url (number &key root-url)
  "Return API URL to receive the NUMBER of latest evaluations.
See function `build-farm-url' for the meaning of ROOT-URL."
  (build-farm-api-url
   "evaluations"
   `(("nr" . ,number))
   :root-url root-url))


;;; Receiving data from a build farm

(defvar url-http-codes)

(defun build-farm-retrieve-url (url)
  "Retrieve URL synchronously and return buffer containing the data.
This function is similar to `url-retrieve-synchronously' but it
also raises an error if URL has not been retrieved properly."
  ;; This code is taken from `url-insert-file-contents'.
  (let ((buffer (url-retrieve-synchronously url)))
    (unless buffer
      (signal 'file-error (list url "No Data")))
    (with-current-buffer buffer
      (when (bound-and-true-p url-http-response-status)
        (unless (and (>= url-http-response-status 200)
                     (< url-http-response-status 300))
          (let ((desc (nth 2 (assq url-http-response-status
                                   url-http-codes))))
            (kill-buffer buffer)
            (signal 'file-error (list url desc))))))
    buffer))

(defun build-farm-receive-data (url)
  "Return output received from URL and processed with `json-read'."
  (let* ((url-request-extra-headers '(("Accept" . "application/json")))
         (url-buffer (build-farm-retrieve-url url))
         (content-type (buffer-local-value 'url-http-content-type
                                           url-buffer)))
    ;; Do not use `string=' here because the content type may look like
    ;; this: "application/json;charset=utf-8".
    (unless (string-match-p "application/json" content-type)
      ;; Currently Cuirass does not support "Accept" extra header, so it
      ;; does not return json data from "non-api" URLs.
      (if (eq (build-farm-url-type) 'cuirass)
          (error "Sorry, Cuirass does not support this API")
        (error "\
The server has not returned 'application/json' content type.
Perhaps, API has changed:\n%s"
               url)))
    (with-temp-buffer
      (url-insert-buffer-contents url-buffer url)
      (goto-char (point-min))
      (let ((json-false nil)    ; default value is `:json-false'
            (json-key-type 'symbol)
            (json-array-type 'list)
            (json-object-type 'alist))
        (json-read)))))

(provide 'build-farm-url)

;;; build-farm-url.el ends here
