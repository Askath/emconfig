;; General editor settings
(setq make-backup-files nil)
(setq global-auto-revert-mode 1)
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)

(setq lombok-library-path (concat user-emacs-directory "lombok.jar"))
(unless (file-exists-p lombok-library-path)
  (url-copy-file "https://projectlombok.org/downloads/lombok.jar" lombok-library-path))

(use-package eglot
  :hook
  ((js-ts-mode . eglot-ensure)
   (tsx-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (java-ts-mode . eglot-ensure)
   (html-ts-mode . eglot-ensure)
   (clojure-mode . eglot-ensure)
   (clojurescript-mode . eglot-ensure))
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (eglot--code-action eglot-code-action-organize-imports-ts "source.organizeImports.ts")
  (eglot--code-action eglot-code-action-unused-imports-ts "source.removeUnusedImports.ts")
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs
               `(typescript-ts-mode . ("typescript-language-server" "--stdio" :initializationOptions
                                       (:preferences
                                        (:disableSuggestions :json-false
                                         :quotePreference "double"
                                         :includeCompletionsForModuleExports t
                                         :includeCompletionsForImportStatements t
                                         :includeCompletionsWithSnippetText t
                                         :includeCompletionsWithInsertText t
                                         :includeAutomaticOptionalChainCompletions t
                                         :includeCompletionsWithClassMemberSnippets t
                                         :includeCompletionsWithObjectLiteralMethodSnippets t
                                         :useLabelDetailsInCompletionEntries t
                                         :allowIncompleteCompletions t
                                         :importModuleSpecifierPreference "shortest"
                                         :importModuleSpecifierEnding "minimal"
                                         :allowTextChangesInNewFiles t
                                         :providePrefixAndSuffixTextForRename t
                                         :provideRefactorNotApplicableReason :json-false
                                         :allowRenameOfImportPath t
                                         :jsxAttributeCompletionStyle "auto"
                                         :displayPartsForJSDoc t
                                         :generateReturnInDocTemplate t
                                         :includeInlayParameterNameHints "all"
                                         :includeInlayParameterNameHintsWhenArgumentMatchesName t
                                         :includeInlayFunctionParameterTypeHints t
                                         :includeInlayVariableTypeHints t
                                         :includeInlayVariableTypeHintsWhenTypeMatchesName t
                                         :includeInlayPropertyDeclarationTypeHints t
                                         :includeInlayFunctionLikeReturnTypeHints t
                                         :includeInlayEnumMemberValueHints t
                                         :removeUnusedImports t
                                         :disableLineTextInReferences :json-false)))))


(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

(with-eval-after-load 'org (global-org-modern-mode))
