((nil
  . ((fill-column . 78)
     (tab-width   .  8)
     (sentence-end-double-space . t)
     (eval . (add-to-list 'completion-ignored-extensions ".go"))

     ;; Geiser
     (geiser-insert-actual-lambda . nil)
     ;; This allows automatically setting the `geiser-guile-load-path'
     ;; variable when using various checkouts (e.g., via git worktrees).
     (geiser-repl-per-project-p . t)
     ;; Note this next setting will use the current guix as the geiser binary;
     ;; one working with a local guix checkout may want something different.
     (geiser-guile-binary . ("guix" "repl"))))

 (scheme-mode
  . ((eval . (progn
               (highlight-regexp "(define.+-git$" 'hi-yellow-b)
               (highlight-regexp "(define.+-bin$" 'hi-red-b)
               (highlight-regexp "(define.+-nonfree$" 'hi-red-b)
               (highlight-regexp "nonfree:nonfree" 'hi-red-b)))
     (indent-tabs-mode . nil))))
