#lang typed/racket

(module Language typed/racket
  (define-type ast/LanguageType (U ast/integer32 ast/variable-string))
  (struct ast/integer32 ())
  (struct ast/variable-string ([size : Integer]))

  (define-type ast/FieldMetadata (U ast/position ast/type))
  (struct ast/position ())
  (struct ast/type ([type : ast/LanguageType]))

  (define-type ast/Table (U ast/name ast/attributes))
  (struct ast/name ([value : String]))
  (struct ast/attributes ([map : (Immutable-HashTable Symbol ast/FieldMetadata)]))

  (define-type ast/Entity (U ast/table))
  (struct ast/table ([kind : ast/Table]))

  ;; ---------------------------------------------------------------------

  (: ast/get-byte-size (-> ast/LanguageType Integer))
  (define (ast/get-byte-size x)
    (match x
      [(? ast/integer32?) 4]
      [(? ast/variable-string?) (ast/variable-string-size x)]))


  (ast/name-value (ast/name "abc"))

  ;; ---------------------------------------------------------------------

  (provide ast/integer32)
  (provide ast/variable-string)
  (provide ast/LanguageType)
  (provide ast/FieldMetadata)
  (provide ast/position)
  (provide ast/type)
  (provide ast/Table)
  (provide ast/name)
  (provide ast/attributes)
  (provide ast/Entity)
  (provide ast/table)
  (provide ast/get-byte-size))
