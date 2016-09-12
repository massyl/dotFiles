# Install the following modules
** stylish-haskell**, **hlint**, **hasktags**, **ghc-mod**

```haskell

stack install stylish-haskell hlint hasktags

```

# Add the following configuration to `.spacemacs` (defun dotspacemacs/user-init ())

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'exec-path "~/.local/bin/")

# Install ghc-mod #

stack intall ghc-mod


