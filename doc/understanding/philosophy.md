# Design Philosophy

**eucalypt**, the language, is unorthodox in many respects -- probably
more than you might realise on first acquaintance.

People tend to have deep-seated and inflexible opinions about
programming languages and language design and will quite possibly find
something in here that they have a kneejerk reaction against.

However, the design is not unprincipled and, while it is experimental
in some respects, I believe it's internally consistent. Several
aspects of the design and the aesthetic are driven by the primary use
case, templating and generating YAML. Maybe by exploring some of the
inspiration and philosophy behind the language itself, I can pre-empt
some of the knee jerks.

## Accept crypticality for minimal intrusion

**eucalypt** is first and foremost a *tool*, rather than a language. It is
intended to replace generation and transformation processes on
semi-structured data formats. Many or most uses of **eucalypt** the
language should just be simple one-liner tags in YAML files, or maybe
eucalypt files that are predominantly data rather than manipulation.

The **eucalypt** language is the depth behind these one-liners that
allows **eucalypt** to accommodate increasingly ambitious use cases
without breaking the paradigm and reaching for a general purpose
imperative scripting language or the lowest common denominator of
text-based templating languages.

The pre-eminence of one-liners and small annotations and "logic
mark-up", means that **eucalypt** often favours concise and cryptic over
wordy and transparent. This is a controversial approach.

- **eucalypt** logic should "get out of the way" of the data. Templating
  is attractive precisely because the generating source looks very
  like the result. Template tags are often short (with "cryptic"
  delimiters -- `{{}}`, `<%= %>`, `[| ]`...) because these are "marking
  up" the data which is the main event. At the same time, the tags are
  often "noisy" or visually disruptive to ensure they cannot be
  ignored. **eucalypt** via operator and bracket definitions, picks and
  chooses from a similar palette of expressive effects to try and be a
  sympathetic cohabitee with its accompanying data.

- There are many cases where it makes sense to resist offering an
  incomplete understanding in favour of demanding full understanding.
  For example, it is spurious to say that `bind(x, f)` gives more
  understanding of what is going on than `x >>= f` -- unless you
  understand the monad abstraction and the role of bind in it, you
  gain nothing useful from the ideas that the word `bind` connotes
  when you are trying to understand program text.

- **eucalypt** just plain ignores the notion that program text should
  be readable *as English text*. This (well motivated) idea has made a
  resurgence in recent years through the back door of internal DSLs
  and "fluent" Java interfaces. There is much merit in languages
  supple enough to allow the APIs to approach the natural means of
  expression of the problem domain. However, problem domains
  frequently have their own technical jargon and notation which suit
  their purpose better than natural language so it cuts both ways.
  Program text should be approachable by its target audience but that
  does not mean it should make no demands of its target audience.

These stances lead directly to several slightly esoteric aspects of
**eucalypt** that may be obnoxious to some:

- **eucalypt** tends to be operator-heavy. Operators are concise (if
  cryptic) and the full range of unicode is available to call upon.
  Using operators keeps custom logic visually out of the way of the
  data whilst also signposting it to attract closer attention.

- **eucalypt** lets you define your own operators and specify their
  precedence and associativity (which are applied at a relatively late
  stage in the evaluation pipeline -- *operator soup* persists through
  the initial parse). There are no ternary operators.

- For absolute minimal intrusion, merely the act of placing elements
  next to each other ("catenation"), `x f`, is meaningful in
  **eucalypt**. By default this is pipeline-order function
  application, but blocks can be applied as functions to make common
  transformations, like block merge, very succinct.

- For even more power, **eucalypt** might soon let you alter the
  meaning of concatenation via overloaded *idiot brackets* [^1]. (`«x y»: ...`). This is inspired by the *idiom brackets* that can be used
  to express applicative styles in functional programming [^2]. These
  may also provide an acceptable proxy for ternary and other operators
  too.

- An equivalent generalisation of **eucalypt** block syntax to provide
  a capability similar to Haskell's `do` notation could conceivably
  follow.

## Cohabitation of code and data

Just like templates, **eucalypt** source (or **eucalypt**-tagged YAML)
should be almost entirely data.

The idea behind **eucalypt** is to adopt the basic maps-and-arrays
organisation philosophy of these data formats but make the data
*active* -- allowing lambdas to live in and amongst it and operate on
it and allowing the data to express dispositions towards its
environment by addition of metadata that controls import, export, and
execution preferences.

**eucalypt** therefore collapses the separation of code and data to some
degree. You can run `eu` against a mixture of YAML, JSON and eucalypt
files and all the data and logic appears there together in the same
namespace hierarchy. The namespace hierarchy just *is* the data.

However, code and data aren't unified in the sense of Lisp for
instance. **eucalypt** is not homoiconic. The relationship is more like
cohabitation; code lives in amongst the data it operates on but is
stripped out before export.

Nevertheless **eucalypt** is heavily inspired by Lisp and aims for a
similar fluidity through:

- lazy evaluation (going some way towards matching uses of Lisp macros
  which control evaluation order -- in eucalypt, `if` is just a
  function)
- economical syntax to facilitate (future) manipulation of code as
  data

## Simplicity

- **eucalypt** values simplicity in the sense of fewer moving parts (and
  therefore, hopefully, fewer things to go wrong). It values ease of
  use in the sense of offering a rich and powerful toolkit. You may
  not think it achieves either.

- **eucalypt** values familiarity mostly in the "shallower" parts of
  the language where it only requires a couple of mental leaps for the
  average programmer in these areas -- the (ab)use of catenation being
  the key one.

- However, **eucalypt** isn't ashamed of its dusty corners. Dusty
  corners are areas where novices and experts alike can get trapped
  and lose time but they're also rich seams for experimentation,
  innovation and discovery. If you have to venture too far off-piste
  to find what you need, we'll find a way to bring it onto the nursery
  slopes but we won't close off the mountain.


---

#### Footnotes

[^1]: Inspired by *idiom brackets*. If I didn't call them that,
    someone else would.

[^2]: Applicative Programming with Effects, Conor McBride and Ross
    Paterson. (2008)
    http://www.staff.city.ac.uk/~ross/papers/Applicative.html
