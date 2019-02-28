# purescript-arraybuffer-class

Typeclasses for binary serialization in PureScript.

> Note: Although this library shares a lot of features similar to the [cereal library from Haskell](https://hackage.haskell.org/package/cereal), it is not yet compatible.

## Usage

It should feel fairly similar to [Argonaut](https://pursuit.purescript.org/package/purescript-argonaut), but with a
binary spin on things - everything is very `Effect`-ful, and will throw errors if something doesn't line-up as expected.

Just implement the `EncodeArrayBuffer` and `DecodeArrayBuffer` typeclasses to support binary serialization for
your data type.

## Notes

- The `Array` instance first packs its length as a 4-byte `Uint32BE` - I don't know of a perfect way around this
  without excessive testing.
- The `Char` and `CodePoint` instances seem to reflect utf-8 pretty decently - Woo Hoo! Pure utf-8 serialization!! But wait...
- The `String` instance just turns it into an `Array CodePoint`, so there's an initial word denoting the string's length.
  Right now, I don't know of a clean way around this, without making every instance have a length argument.
- The `Vec` instance actually sidesteps the length argument, because the length is assumed to be known at compile-time.
- The `ArrayBuffer`, `DataView`, and `ArrayView` instances also have a similar initial length argument. Writing
  is not as efficient as you'd hope.
- The `Record` instance first turns the record into an `Object ArrayBuffer`, which then becomes a
  `Array (Tuple String ArrayBuffer)` - it's not super efficient.
- The `Generic` instances treat every `Sum` as its own distinguished, isolated possability - for data types with
  multiple consecutive sum values, like `These` or `Ordering`, it's suggested to write your own instance to pack
  each sum constructor as its own flag in the byte range `0 - 255`. `Sum`s are treated like `Either` - only `0` and
  `1` are used.
