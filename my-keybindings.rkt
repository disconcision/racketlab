#lang s-exp framework/keybinding-lang
(keybinding "c:a" (λ (editor evt) (send editor insert "!")))