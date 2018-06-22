#lang racket

#| 2018.06.15

 a consolidation of notes/sketches on
 syntax annotation and hash patterns

 attributes:
   annotations, aspects, and affordances

 remember to think about this in the context of
 attribute grammars. need more conceptual clarity
 of intrisic versus derived attributes.

|#



#| 2018.06.22
   idea: some degree of snaffo/property duality
   (<props> ... // <pat>) matches syntax
   matching <pat> with properties <props> |#

#| the below represent pattern-templates for moving
   a selector between selectable elements|#

'((// (>> as)) ...
  (// (> b))
  (// (>> c))
  (// (>> d)) ...)
'((// (>> as)) ...
  (// (>> b))
  (// (> >> c)) ; as opposed to nesting (>> (> c))
  (// (>> d)) ...)

'((selectable? // as) ...
  (// (> b))
  (selectable? // c)
  (selectable? // ds) ...)
'((selectable? // as) ...
  (selectable? // b)
  (// (> c))
  (selectable? // ds) ...)

'((selectable? // as) ...
  (selected? // b)
  (selectable? // c)
  (selectable? // ds) ...)
'((selectable? // as) ...
  (selectable? // b)
  (selected? // c)
  (selectable? // ds) ...)




#| NOTES FROM containment-pattern-sketch.rkt |#

; side note: hash pattern syntax?
#; (h : rest-sexpr ...) ; bind h to hash
#; (h tag ... : rest-sexpr ...) ; where tag means check (has-has-key tag)
; e.g.
#; (h expr? : if true 0 1)
#; (h (or tag (prop . <pat>)) ... : rest-sexpr ...)
; (destructure (hash-ref h prop) <pat>)
; e.g.
#; (h (type . N) : if true b c)
; binding hash is optional? (actually this makes the syntax ambiguous...)
; find some way to make it work:
#; (tag tag tag : rest ...)
#; ((prop . value) : rest ...)
; in template:
#; (h (prop : (make-new-val value)))



#| NOTES FROM fructerm.rkt |#

; annotation patterns

#; ([attribute : pattern] rest-of list contents)

#| this matches against a list beginning with a hash containing key 'attribute' whose value matches pattern |#

; but if given a list that doesn't start with list containing a colon as the second element e.g. (a b c),
; this is re-written as (,any-hash a b c)

; attributes are unique and are keywordish. if we try to match
#; (attribute rest-of list contents ...)
#; (attribute arg ... : rest-of list contents ...)
; this gets rewritten to
#; ([attribute : _] rest-of list contents ...)
#; ([attribute : arg ...] rest-of list contents ....)

; e.g.

#; (▹ ,a)
#; ([▹ :] ,sel)

#; (s▹ ,buf : ,sel)
#; ([s▹ : ,buf] ,sel)

; maybe
#; (s: ,buf : ,sel)
#; ([s : ,buf] ,sel)

; alternatively we could 'project' an affordance before matching on it:
; (term-rewrite (app (project-attribute ▹) `(▹ ,a)) ((inject-attribute ▹) `(▹ ,a))


; metafunctionality

; function to automatically left-right reverse a rewrite rule