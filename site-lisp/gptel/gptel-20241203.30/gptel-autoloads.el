;;; gptel-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from gptel.el

(autoload 'gptel-mode "gptel" "\
Minor mode for interacting with LLMs.

This is a minor mode.  If called interactively, toggle the `GPTel mode'
mode.  If the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `gptel-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(autoload 'gptel-send "gptel" "\
Submit this prompt to the current LLM backend.

By default, the contents of the buffer up to the cursor position
are sent.  If the region is active, its contents are sent
instead.

The response from the LLM is inserted below the cursor position
at the time of sending.  To change this behavior or model
parameters, use prefix arg ARG activate a transient menu with
more options instead.

This command is asynchronous, you can continue to use Emacs while
waiting for the response.

(fn &optional ARG)" t)
(autoload 'gptel "gptel" "\
Switch to or start a chat session with NAME.

Ask for API-KEY if `gptel-api-key' is unset.

If region is active, use it as the INITIAL prompt.  Returns the
buffer created or switched to.

INTERACTIVEP is t when gptel is called interactively.

(fn NAME &optional _ INITIAL INTERACTIVEP)" t)
(register-definition-prefixes "gptel" '("gptel-"))


;;; Generated autoloads from gptel-anthropic.el

(autoload 'gptel-make-anthropic "gptel-anthropic" "\
Register an Anthropic API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.anthropic.com\" by default.

MODELS is a list of available model names, as symbols.
Additionally, you can specify supported LLM capabilities like
vision or tool-use by appending a plist to the model with more
information, in the form

 (model-name . plist)

For a list of currently recognized plist keys, see
`gptel--anthropic-models'. An example of a model specification
including both kinds of specs:

:models
\\='(claude-3-haiku-20240307               ;Simple specs
  claude-3-opus-20240229
  (claude-3-5-sonnet-20240620           ;Full spec
   :description  \"Balance of intelligence and speed\"
   :capabilities (media tool json)
   :mime-types
   (\"image/jpeg\" \"image/png\" \"image/gif\" \"image/webp\")))

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/messages\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY is a variable whose value is the API key, or function that
returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for.

(fn NAME &key CURL-ARGS STREAM KEY REQUEST-PARAMS (HEADER (lambda nil (when-let (key (gptel--get-api-key)) \\=`((\"x-api-key\" \\=\\, key) (\"anthropic-version\" . \"2023-06-01\") (\"anthropic-beta\" . \"pdfs-2024-09-25\") (\"anthropic-beta\" . \"prompt-caching-2024-07-31\"))))) (MODELS gptel--anthropic-models) (HOST \"api.anthropic.com\") (PROTOCOL \"https\") (ENDPOINT \"/v1/messages\"))")
(function-put 'gptel-make-anthropic 'lisp-indent-function 1)
(register-definition-prefixes "gptel-anthropic" '("gptel--anthropic-"))


;;; Generated autoloads from gptel-context.el

 (autoload 'gptel-add "gptel-context" "Add/remove regions or buffers from gptel's context." t)
 (autoload 'gptel-add-file "gptel-context" "Add files to gptel's context." t)
(autoload 'gptel-context--wrap "gptel-context" "\
Wrap MESSAGE with context string.

(fn MESSAGE)")
(autoload 'gptel-context--collect "gptel-context" "\
Get the list of all active context overlays.")
(register-definition-prefixes "gptel-context" '("gptel-"))


;;; Generated autoloads from gptel-curl.el

(autoload 'gptel-curl-get-response "gptel-curl" "\
Retrieve response to prompt in INFO.

INFO is a plist with the following keys:
- :data (the data being sent)
- :buffer (the gptel buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point.

(fn INFO &optional CALLBACK)")
(register-definition-prefixes "gptel-curl" '("gptel-curl--"))


;;; Generated autoloads from gptel-gemini.el

(autoload 'gptel-make-gemini "gptel-gemini" "\
Register a Gemini backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, defaults to
\"generativelanguage.googleapis.com\".

MODELS is a list of available model names, as symbols.
Additionally, you can specify supported LLM capabilities like
vision or tool-use by appending a plist to the model with more
information, in the form

 (model-name . plist)

For a list of currently recognized plist keys, see
`gptel--gemini-models'. An example of a model specification
including both kinds of specs:

:models
\\='(gemini-pro                            ;Simple specs
  gemini-1.5-flash
  (gemini-1.5-pro-latest                ;Full spec
   :description
   \"Complex reasoning tasks, problem solving and data extraction\"
   :capabilities (tool json)
   :mime-types
   (\"image/jpeg\" \"image/png\" \"image/webp\" \"image/heic\")))


STREAM is a boolean to enable streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, \"https\" by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1beta/models\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for.

(fn NAME &key CURL-ARGS HEADER KEY REQUEST-PARAMS (STREAM nil) (HOST \"generativelanguage.googleapis.com\") (PROTOCOL \"https\") (MODELS gptel--gemini-models) (ENDPOINT \"/v1beta/models\"))")
(function-put 'gptel-make-gemini 'lisp-indent-function 1)
(register-definition-prefixes "gptel-gemini" '("gptel--gemini-"))


;;; Generated autoloads from gptel-kagi.el

(autoload 'gptel-make-kagi "gptel-kagi" "\
Register a Kagi FastGPT backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is the Kagi host (with port), defaults to \"kagi.com\".

MODELS is a list of available Kagi models: only fastgpt is supported.

STREAM is a boolean to toggle streaming responses, defaults to
false.  Kagi does not support a streaming API yet.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v0/fastgpt\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

Example:
-------

 (gptel-make-kagi \"Kagi\" :key my-kagi-key)

(fn NAME &key CURL-ARGS STREAM KEY (HOST \"kagi.com\") (HEADER (lambda nil \\=`((\"Authorization\" \\=\\, (concat \"Bot \" (gptel--get-api-key)))))) (MODELS \\='((fastgpt :capabilities (nosystem)) (summarize:cecil :capabilities (nosystem)) (summarize:agnes :capabilities (nosystem)) (summarize:daphne :capabilities (nosystem)) (summarize:muriel :capabilities (nosystem)))) (PROTOCOL \"https\") (ENDPOINT \"/api/v0/\"))")
(function-put 'gptel-make-kagi 'lisp-indent-function 1)


;;; Generated autoloads from gptel-ollama.el

(autoload 'gptel-make-ollama "gptel-ollama" "\
Register an Ollama backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is where Ollama runs (with port), defaults to localhost:11434

MODELS is a list of available model names, as symbols.
Additionally, you can specify supported LLM capabilities like
vision or tool-use by appending a plist to the model with more
information, in the form

 (model-name . plist)

Currently recognized plist keys are :description, :capabilities
and :mime-types.  An example of a model specification including
both kinds of specs:

:models
\\='(mistral:latest                        ;Simple specs
  openhermes:latest
  (llava:13b                            ;Full spec
   :description
   \"Llava 1.6: Large Lanuage and Vision Assistant\"
   :capabilities (media)
   :mime-types (\"image/jpeg\" \"image/png\")))


STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, http by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/generate\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.  This is typically not required
for local models like Ollama.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for.

Example:
-------

 (gptel-make-ollama
   \"Ollama\"
   :host \"localhost:11434\"
   :models \\='(mistral:latest)
   :stream t)

(fn NAME &key CURL-ARGS HEADER KEY MODELS STREAM REQUEST-PARAMS (HOST \"localhost:11434\") (PROTOCOL \"http\") (ENDPOINT \"/api/chat\"))")
(function-put 'gptel-make-ollama 'lisp-indent-function 1)
(register-definition-prefixes "gptel-ollama" '("gptel--ollama-"))


;;; Generated autoloads from gptel-openai.el

(autoload 'gptel-make-openai "gptel-openai" "\
Register an OpenAI API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, typically \"api.openai.com\".

MODELS is a list of available model names, as symbols.
Additionally, you can specify supported LLM capabilities like
vision or tool-use by appending a plist to the model with more
information, in the form

 (model-name . plist)

For a list of currently recognized plist keys, see
`gptel--openai-models'. An example of a model specification
including both kinds of specs:

:models
\\='(gpt-3.5-turbo                         ;Simple specs
  gpt-4-turbo
  (gpt-4o-mini                          ;Full spec
   :description
   \"Affordable and intelligent small model for lightweight tasks\"
   :capabilities (media tool json url)
   :mime-types
   (\"image/jpeg\" \"image/png\" \"image/gif\" \"image/webp\")))

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/chat/completions\".

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for.

(fn NAME &key CURL-ARGS MODELS STREAM KEY REQUEST-PARAMS (HEADER (lambda nil (when-let (key (gptel--get-api-key)) \\=`((\"Authorization\" \\=\\, (concat \"Bearer \" key)))))) (HOST \"api.openai.com\") (PROTOCOL \"https\") (ENDPOINT \"/v1/chat/completions\"))")
(function-put 'gptel-make-openai 'lisp-indent-function 1)
(autoload 'gptel-make-azure "gptel-openai" "\
Register an Azure backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is the API host.

MODELS is a list of available model names, as symbols.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT is the API endpoint for completions.

HEADER (optional) is for additional headers to send with each
request.  It should be an alist or a function that retuns an
alist, like:
 ((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for.

Example:
-------

 (gptel-make-azure
  \"Azure-1\"
  :protocol \"https\"
  :host \"RESOURCE_NAME.openai.azure.com\"
  :endpoint
  \"/openai/deployments/DEPLOYMENT_NAME/completions?api-version=2023-05-15\"
  :stream t
  :models \\='(gpt-3.5-turbo gpt-4))

(fn NAME &key CURL-ARGS HOST (PROTOCOL \"https\") (HEADER (lambda nil \\=`((\"api-key\" \\=\\, (gptel--get-api-key))))) (KEY \\='gptel-api-key) MODELS STREAM ENDPOINT REQUEST-PARAMS)")
(function-put 'gptel-make-azure 'lisp-indent-function 1)
(defalias 'gptel-make-gpt4all 'gptel-make-openai "\
Register a GPT4All backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST is where GPT4All runs (with port), typically localhost:4891

MODELS is a list of available model names, as symbols.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/api/v1/completions\"

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY (optional) is a variable whose value is the API key, or
function that returns the key. This is typically not required for
local models like GPT4All.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for.

Example:
-------

(gptel-make-gpt4all
 \"GPT4All\"
 :protocol \"http\"
 :host \"localhost:4891\"
 :models \\='(mistral-7b-openorca.Q4_0.gguf))")
(register-definition-prefixes "gptel-openai" '("gptel--"))


;;; Generated autoloads from gptel-org.el

(register-definition-prefixes "gptel-org" '("gptel-"))


;;; Generated autoloads from gptel-privategpt.el

(autoload 'gptel-make-privategpt "gptel-privategpt" "\
Register an Privategpt API-compatible backend for gptel with NAME.

Keyword arguments:

CURL-ARGS (optional) is a list of additional Curl arguments.

HOST (optional) is the API host, \"api.privategpt.com\" by default.

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to
false.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions, defaults to
\"/v1/messages\".

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that retuns an
alist, like:
((\"Content-Type\" . \"application/json\"))

KEY is a variable whose value is the API key, or function that
returns the key.

CONTEXT and SOURCES: if true (the default), use available context
and provide sources used by the model to generate the response.

REQUEST-PARAMS (optional) is a plist of additional HTTP request
parameters (as plist keys) and values supported by the API.  Use
these to set parameters that gptel does not provide user options
for.

(fn NAME &key CURL-ARGS STREAM KEY REQUEST-PARAMS (HEADER (lambda nil (when-let (key (gptel--get-api-key)) \\=`((\"Authorization\" \\=\\, (concat \"Bearer \" key)))))) (HOST \"localhost:8001\") (PROTOCOL \"http\") (MODELS \\='(private-gpt)) (ENDPOINT \"/v1/chat/completions\") (CONTEXT t) (SOURCES t))")
(function-put 'gptel-make-privategpt 'lisp-indent-function 1)
(register-definition-prefixes "gptel-privategpt" '("gptel--privategpt-parse-sources"))


;;; Generated autoloads from gptel-rewrite.el

 (autoload 'gptel-rewrite "gptel-rewrite" nil t)
(register-definition-prefixes "gptel-rewrite" '("gptel-"))


;;; Generated autoloads from gptel-transient.el

 (autoload 'gptel-menu "gptel-transient" nil t)
 (autoload 'gptel-system-prompt "gptel-transient" nil t)
(register-definition-prefixes "gptel-transient" '("gptel-"))

;;; End of scraped data

(provide 'gptel-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; gptel-autoloads.el ends here
