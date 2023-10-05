# elec-pair-extra

Apply extra rules to alter elec-pair behaviors.

## Customization

- `elec-pair-extra-rules`

```
A alist of major-mode and related rule. Each element is in the form of

(MODE :pair    ( CHAR/(CHAR . PAIR-CHAR) ...)
      :inhibit ( CHAR/(CHAR . REGEXP/FUNCTION) ...)).

MODE: A major mode.

CHAR: A character to match the input. for example: ?{

(CHAR . PAIR-CHAR): A pair of paired character. for example: (?{ . ?})

(CHAR . REGEXP): A character to match input, a regex pattern for inhibit
                 predicate by `looking-back'. for example: (?{ . \":{\")

(CHAR . FUNCTION): A character to match input, a function accept the input
                   as parameter for inhibit predict. for example:
                   (?{ . (lambda (_c) (eq ?: (char-before (1- (point))))))
```

## Usage

```elisp
(require 'elec-pair-extra)
(add-hook 'after-init-hook #'elec-pair-extra-setup)

;; set rules for major-mode
(setq elec-pair-extra-rules
      '(;; enable <> auto pair for generics usage in typescript, disable pair
        ;; when ?< following a space
        (typescript-ts-mode :pair ((?\< . ?\>)) :inhibit ((?\< . " <")))
        ;; disable pair <> in HTML+JS submode in mhtml-mode
        (mhtml-mode :pair nil :inhibit ((?\< . (lambda (c)
                                                 (and (eq major-mode 'js-mode)
                                                      (= c ?<))))))))
```
