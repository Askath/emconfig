;; Added lombok for java dev
(setq lombok-library-path (concat user-emacs-directory "lombok.jar"))
(unless (file-exists-p lombok-library-path)
  (url-copy-file "https://projectlombok.org/downloads/lombok.jar" lombok-library-path))

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn on eglot for selected modes
  :hook
  ((js-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (java-ts-mode . eglot-ensure)
   (html-ts-mode . eglot-ensure)
   (clojure-mode . eglot-ensure)
   (clojurescript-mode . eglot-ensure)
   )

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event
  (add-to-list 'eglot-server-programs
               `(java-ts-mode . ("jdtls" "-data" "~/jdtls"
                                 "-javaagent:" ,lombok-library-path
                                 "-Xbootclasspath/a:" ,lombok-library-path
                                 "--jvm-arg=-XX:+UseG1GC"
                                 "--jvm-arg=-XX:+UseStringDeduplication"
                                 "-Djava.format.settings.url=file:///home/user/code-format.xml"
                                 "-Djava.format.settings.profile=myown")))
  (add-to-list 'eglot-server-programs
               `(html-ts-mode . ("node" "/opt/homebrew/lib/node_modules/@angular/language-server/" "--ngProbeLocations"
                                 "/opt/homebrew/lib/node_modules" "--tsProbeLocations"
                                 "/opt/homebrew/lib/node_modules/" "--stdio"))))


(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

