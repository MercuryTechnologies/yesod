# ChangeLog for yesod-core

## 1.6.28.0

* Allow users to control generation of the `resourcesSite :: [ResourceTree String]` value. [#1881](https://github.com/yesodweb/yesod/pull/1881)

## 1.6.27.1

* Set `base >= 4.11` for less CPP and imports [#1876](https://github.com/yesodweb/yesod/pull/1876)

## 1.6.27.0

* Build with `wai-extra-3.1.17` [#1861](https://github.com/yesodweb/yesod/pull/1861)

## 1.6.26.0

* Always apply jsAttributes to julius script blocks of body [#1836](https://github.com/yesodweb/yesod/pull/1836)

## 1.6.25.1

* Export the options that were created in 1.6.25.0 [#1825](https://github.com/yesodweb/yesod/pull/1825)

## 1.6.25.0

* Add an options structure that allows the user to set which instances will be derived for a routes structure. [#1819](https://github.com/yesodweb/yesod/pull/1819)

## 1.6.24.5

* Support Aeson 2.2 [#1818](https://github.com/yesodweb/yesod/pull/1818)

## 1.6.24.4

* Fix test-suite compilation error for GHC >= 9.0.1 [#1812](https://github.com/yesodweb/yesod/pull/1812)

## 1.6.24.3

* Fix subsite-to-subsite dispatch [#1805](https://github.com/yesodweb/yesod/pull/1805)

## 1.6.24.2

* No star is type [#1797](https://github.com/yesodweb/yesod/pull/1797)

## 1.6.24.1

* Adapt to removal of `ListT` from transformers-0.6. [#1796](https://github.com/yesodweb/yesod/pull/1796)

## 1.6.24.0

* Make catching exceptions configurable and set the default back to rethrowing async exceptions. [#1772](https://github.com/yesodweb/yesod/pull/1772).

## 1.6.23.1

* Fix typo in creation of the description `<meta>` tag in `defaultLayout`. [#1766](https://github.com/yesodweb/yesod/pull/1766)

## 1.6.23

* Add idempotent versions of `setDescription`, `setDescriptionI`. These functions
  have odd behaviour when called multiple times, so they are now warned against.
  This can't be a silent change - if you want to switch to the new functions, make
  sure your layouts are updated to use `pageDescription` as well as `pageTitle`.
  [#1765](https://github.com/yesodweb/yesod/pull/1765)

## 1.6.22.1

+ Remove sometimes failing superfluous test. [#1756](https://github.com/yesodweb/yesod/pull/1756)

## 1.6.22.0

* Add missing list to documentation for ``Yesod.Core.Dispatch.warp``. [#1745](https://github.com/yesodweb/yesod/pull/1745)
* Add instances for `ToContent Void`, `ToTypedContent Void`. [#1752](https://github.com/yesodweb/yesod/pull/1752)
* Handle async exceptions within yesod rather then warp. [#1753](https://github.com/yesodweb/yesod/pull/1753)
* Support template-haskell 2.18 [#1754](https://github.com/yesodweb/yesod/pull/1754)

## 1.6.21.0

* Export `Yesod.Core.Dispatch.defaultGen` so that users may reuse it for their own `YesodRunnerEnv`s [#1734](https://github.com/yesodweb/yesod/pull/1734)

## 1.6.20.2

* Fix compatibility with template-haskell 2.17 [#1729](https://github.com/yesodweb/yesod/pull/1729)

## 1.6.20.1

* Throw an error in `breadcrumbs` if the trail of breadcrumbs is circular. [#1727](https://github.com/yesodweb/yesod/issues/1727)

## 1.6.20

* Generate CSRF tokens using a secure entropy source [#1726](https://github.com/yesodweb/yesod/pull/1726)
* Change semantics of `yreGen` and `defaultGen`

## 1.6.19.0

* Change order of priority in `languages`[#1721](https://github.com/yesodweb/yesod/pull/1721)

## 1.6.18.8

* Fix test suite for wai-extra change around vary header

## 1.6.18.7

* Fix functions generating Open Graph metadata[#1709](https://github.com/yesodweb/yesod/pull/1709)

## 1.6.18.6

* Update documentation from `HandlerT` to `HandlerFor` [#1703](https://github.com/yesodweb/yesod/pull/1703)

## 1.6.18.5

Document `ErrorResponse` [#1698](https://github.com/yesodweb/yesod/pull/1698)

## 1.6.18.4

* Fixed a bug where `mkYesod` and other TH functions didn't work for datatypes with explicitly stated type variables, including the case with typeclass constraints. [https://github.com/yesodweb/yesod/pull/1697](#1697)

## 1.6.18.3

* Remove mention of an oudated Yesod type (`GHandler`) from the docs for `handlerToIO`. [https://github.com/yesodweb/yesod/pull/1695](#1695)

## 1.6.18.2

* Recommends `.yesodroutes` as the file extension for Yesod routes files. [#1686](https://github.com/yesodweb/yesod/pull/1686)

## 1.6.18.1

* Increase the size of CSRF token

## 1.6.18

* Add functions for setting description and OG meta [#1663](https://github.com/yesodweb/yesod/pull/1663)

* Use `DeriveLift` to implement the `Lift` instances for `ResourceTree`,
  `Resource`, `Piece`, and `Dispatch`. Among other benefits, this provides
  implementations of `liftTyped` on `template-haskell-2.16` (GHC 8.10) or
  later. [#1664](https://github.com/yesodweb/yesod/pull/1664)

## 1.6.17.3

* Support for `unliftio-core` 0.2

## 1.6.17.2

* Support template-haskell 2.16, build with GHC 8.10 [#1657](https://github.com/yesodweb/yesod/pull/1657)

## 1.6.17.1

* Remove unnecessary deriving of Typeable

## 1.6.17

* Adds `contentTypeIsJson` [#1646](https://github.com/yesodweb/yesod/pull/1646)

## 1.6.16.1

* Compiles with GHC 8.8.1

## 1.6.16

* Add `jsAttributesHandler` to run arbitrary Handler code before building the
  attributes map for the script tag generated by `widgetFile` [#1622](https://github.com/yesodweb/yesod/pull/1622)

## 1.6.15

* Move `redirectToPost` JavaScript form submission from HTML element to
  `<script>` tag for CSP reasons [#1620](https://github.com/yesodweb/yesod/pull/1620)

## 1.6.14

* Introduce `JSONResponse`. [issue #1481](https://github.com/yesodweb/yesod/issues/1481) and [PR #1592](https://github.com/yesodweb/yesod/pull/1592)

## 1.6.13

* Introduce `maxContentLengthIO`. [issue #1588](https://github.com/yesodweb/yesod/issues/1588) and [PR #1589](https://github.com/yesodweb/yesod/pull/1589)

## 1.6.12

* Use at most one valid session cookie per request [#1581](https://github.com/yesodweb/yesod/pull/1581)

## 1.6.11

* Deprecate insecure JSON parsing functions [#1576](https://github.com/yesodweb/yesod/pull/1576)

## 1.6.10.1

* Fix test suite compilation for [commercialhaskell/stackage#4319](https://github.com/commercialhaskell/stackage/issues/4319)

## 1.6.10

* Adds functions to get and set values in the per-request caches. [#1573](https://github.com/yesodweb/yesod/pull/1573)

## 1.6.9

* Add `sendResponseNoContent` [#1565](https://github.com/yesodweb/yesod/pull/1565)

## 1.6.8.1

* Add missing test file to tarball [#1563](https://github.com/yesodweb/yesod/issues/1563)

## 1.6.8
* In the route syntax, allow trailing backslashes to indicate line
    continuation. [#1558](https://github.com/yesodweb/yesod/pull/1558)

## 1.6.7

* If no matches are found, `selectRep` chooses first representation regardless
    of the presence or absence of a `Content-Type` header in the request
    [#1540](https://github.com/yesodweb/yesod/pull/1540)
* Sets the `X-XSS-Protection` header to `1; mode=block` [#1550](https://github.com/yesodweb/yesod/pull/1550)
* Add `PrimMonad` instances for `HandlerFor` and `WidgetFor` [from
  StackOverflow](https://stackoverflow.com/q/52692508/369198)

## 1.6.6

* `defaultErrorHandler` handles text/plain requests [#1522](https://github.com/yesodweb/yesod/pull/1520)

## 1.6.5

* Add `fileSourceByteString` [#1503](https://github.com/yesodweb/yesod/pull/1503)

## 1.6.4

* Add `addContentDispositionFileName` [#1504](https://github.com/yesodweb/yesod/pull/1504)

## 1.6.3

* Add missing export for `SubHandlerFor`

## 1.6.2

* Derive a `Show` instance for `ResourceTree` and `FlatResource` [#1492](https://github.com/yesodweb/yesod/pull/1492)
	* Some third party packages, like `yesod-routes-flow` derive their own `Show` instance, and this will break those packages.

## 1.6.1

* Add a `Semigroup LiteApp` instance, and explicitly define `(<>)` in the
  already existing `Semigroup` instances for `WidgetFor`, `Head`, `Body`,
  `GWData`, and `UniqueList`.

## 1.6.0

* Upgrade to conduit 1.3.0
* Switch to `MonadUnliftIO`
* Drop `mwc-random` and `blaze-builder` dependencies
* Strictify some internal data structures
* Add `CI` wrapper to first field in `Header` data constructor
  [#1418](https://github.com/yesodweb/yesod/issues/1418)
* Internal only change, users of stable API are unaffected: `WidgetT`
  holds its data in an `IORef` so that it is isomorphic to `ReaderT`,
  avoiding state-loss issues..
* Overhaul of `HandlerT`/`WidgetT` to no longer be transformers.
* Fix Haddock comment & simplify implementation for `contentTypeTypes` [#1476](https://github.com/yesodweb/yesod/issues/1476)

## 1.4.37.3

* Improve error message when request body is too large [#1477](https://github.com/yesodweb/yesod/pull/1477)

## 1.4.37.2

* Improve error messages for the CSRF checking functions [#1455](https://github.com/yesodweb/yesod/issues/1455)

## 1.4.37.1

* Fix documentation on `languages` function, update `getMessageRender` to use said function. [#1457](https://github.com/yesodweb/yesod/pull/1457)

## 1.4.37

* Add `setWeakEtag` function in Yesod.Core.Handler module.

## 1.4.36

* Add `replaceOrAddHeader` function in Yesod.Core.Handler module. [1416](https://github.com/yesodweb/yesod/issues/1416)

## 1.4.35.1

* TH fix for GHC 8.2

## 1.4.35

* Contexts can be included in generated TH instances. [1365](https://github.com/yesodweb/yesod/issues/1365)
* Type variables can be included in routes.

## 1.4.34

* Add `WaiSubsiteWithAuth`. [#1394](https://github.com/yesodweb/yesod/pull/1394)

## 1.4.33

* Adds curly brackets to route parser. [#1363](https://github.com/yesodweb/yesod/pull/1363)

## 1.4.32

* Fix warnings
* Route parsing handles CRLF line endings
* Add 'getPostParams' in Yesod.Core.Handler
* Haddock rendering improved.

## 1.4.31

* Add `parseCheckJsonBody` and `requireCheckJsonBody`

## 1.4.30

* Add `defaultMessageWidget`

## 1.4.29

* Exports some internals and fix version bounds [#1318](https://github.com/yesodweb/yesod/pull/1318)

## 1.4.28

* Add ToWidget instances for strict text, lazy text, and text builder [#1310](https://github.com/yesodweb/yesod/pull/1310)

## 1.4.27

* Added `jsAttributes` [#1308](https://github.com/yesodweb/yesod/pull/1308)

## 1.4.26

* Modify `languages` so that, if you previously called `setLanguage`, the newly
  set language will be reflected.

## 1.4.25

* Add instance of MonadHandler and MonadWidget for ExceptT [#1278](https://github.com/yesodweb/yesod/pull/1278)

## 1.4.24

* cached and cachedBy will not overwrite global state changes [#1268](https://github.com/yesodweb/yesod/pull/1268)

## 1.4.23.1

* Don't allow sending multiple cookies with the same name to the client, in accordance with [RFC 6265](https://tools.ietf.org/html/rfc6265). This fixes an issue where multiple CSRF tokens were sent to the client. [#1258](https://github.com/yesodweb/yesod/pull/1258)
* Default CSRF tokens to the root path "/", fixing an issue where multiple tokens were stored in cookies, and using the wrong one led to CSRF errors [#1248](https://github.com/yesodweb/yesod/pull/1248)

## 1.4.23

* urlParamRenderOverride method for Yesod class [#1257](https://github.com/yesodweb/yesod/pull/1257)
* Add laxSameSiteSessions and strictSameSiteSessions [#1226](https://github.com/yesodweb/yesod/pull/1226)

## 1.4.22

* Proper handling of impure exceptions within `HandlerError` values

## 1.4.21

* Add support for `Encoding` from `aeson-0.11` [#1241](https://github.com/yesodweb/yesod/pull/1241)

## 1.4.20.2

* GHC 8 support

## 1.4.20.1

* Log a warning when a CSRF error occurs [#1200](https://github.com/yesodweb/yesod/pull/1200)

## 1.4.20

* `addMessage`, `addMessageI`, and `getMessages` functions

## 1.4.19.1

* Allow lines of dashes in route files [#1182](https://github.com/yesodweb/yesod/pull/1182)

## 1.4.19

* Auth logout not working with defaultCsrfMiddleware [#1151](https://github.com/yesodweb/yesod/issues/1151)

## 1.4.18.2

* Allow subsites within hierarchical routes [#1144](https://github.com/yesodweb/yesod/pull/1144)

## 1.4.18

* Add hook to apply arbitrary function to all handlers [#1122](https://github.com/yesodweb/yesod/pull/1122)

## 1.4.17

* Add `getApprootText`

## 1.4.16

* Add `guessApproot` and `guessApprootOr`

## 1.4.15.1

* bugfix neverExpires leaked threads

## 1.4.15

* mkYesod avoids using reify when it isn't necessary. This avoids needing to define the site type below the call to mkYesod.

## 1.4.14

* Add CSRF protection functions and middleware based on HTTP cookies and headers [#1017](https://github.com/yesodweb/yesod/pull/1017)
* Add mkYesodWith, which allows creating sites with polymorphic type parameters [#1055](https://github.com/yesodweb/yesod/pull/1055)
* Do not define the site type below a call to mkYesod (or any variant), as it will be required at splicing time for reification.
  This was allowed before because reification was not in use. Reification was introduced to allow parametrized types to be used
  by mkYesod (and variants), with potentially polymorphic variables.

## 1.4.13

* Add getsYesod function [#1042](https://github.com/yesodweb/yesod/pull/1042)
* Add IsString instance for WidgetT site m () [#1038](https://github.com/yesodweb/yesod/pull/1038)

## 1.4.12

* Don't show source location for logs that don't have that information [#1027](https://github.com/yesodweb/yesod/pull/1027)

## 1.4.11

* Expose `stripHandlerT` and `subHelper`

## 1.4.10

* Export log formatting [#1001](https://github.com/yesodweb/yesod/pull/1001)

## 1.4.9.1

* Deal better with multiple cookie headers

## 1.4.9

* Add simple authentication helpers [#962](https://github.com/yesodweb/yesod/pull/962)

## 1.4.8.3

* Use 307 redirect for cleaning paths and non-GET requests [#951](https://github.com/yesodweb/yesod/issues/951)

## 1.4.8.2

* Allow blaze-builder 0.4

## 1.4.8.1

* Bump upper bound on path-pieces

## 1.4.8

* Add a bunch of `Semigroup` instances

## 1.4.7.3

* Remove defunct reference to SpecialResponse [#925](https://github.com/yesodweb/yesod/issues/925)

## 1.4.7

SSL-only session security [#894](https://github.com/yesodweb/yesod/pull/894)

## 1.4.6.2

monad-control 1.0

## 1.4.6

Added the `Yesod.Core.Unsafe` module.

## 1.4.5

* `envClientSessionBackend`
* Add `MonadLoggerIO` instances (conditional on monad-logger 0.3.10 being used).

## 1.4.4.5

Support time 1.5

## 1.4.4.2

`neverExpires` uses dates one year in the future (instead of in 2037).

## 1.4.4.1

Improvements to etag/if-none-match support #868 #869

## 1.4.4

Add the `notModified` and `setEtag` functions.

## 1.4.3

Switch to mwc-random for token generation.
