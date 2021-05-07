;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
  (intero-targets "eucalypt:lib" "eucalypt:exe:eu" "eucalypt:test:eucalypt-test")))



((nil . ((projectile-project-compilation-cmd . "stack build --test --fast --file-watch --copy-bins --exec 'hlint .'"))))
