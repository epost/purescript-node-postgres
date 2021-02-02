module Test.Main where

import Prelude
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Console (log)
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Test.Assert (assert')

test ∷ String → Effect Boolean → Effect Unit
test s k = k >>= \r → assert' s r *> log ("[OK] " <> s)

test_tryRead_full ∷ Effect Unit
test_tryRead_full = test "tryRead/full" do
  var ← AVar.new "foo"
  val1 ← AVar.tryRead var
  val2 ← AVar.tryRead var
  pure (val1 == Just "foo" && val2 == Just "foo")

test_tryRead_empty ∷ Effect Unit
test_tryRead_empty = test "tryRead/empty" do
  var ← AVar.empty
  val1 ∷ Maybe Unit ← AVar.tryRead var
  pure (val1 == Nothing)

test_tryPut_full ∷ Effect Unit
test_tryPut_full = test "tryPut/full" do
  var ← AVar.new "foo"
  res ← AVar.tryPut "bar" var
  pure (not res)

test_tryPut_empty ∷ Effect Unit
test_tryPut_empty = test "tryPut/empty" do
  var ← AVar.empty
  res ← AVar.tryPut "foo" var
  val ← AVar.tryRead var
  pure (res && val == Just "foo")

test_tryTake_full ∷ Effect Unit
test_tryTake_full = test "tryTake/full" do
  var ← AVar.new "foo"
  res1 ← AVar.tryTake var
  res2 ← AVar.tryTake var
  pure (res1 == Just "foo" && res2 == Nothing)

test_tryTake_empty ∷ Effect Unit
test_tryTake_empty = test "tryTake/empty" do
  var  ← AVar.empty
  res1 ← AVar.tryTake var
  res2 ← AVar.tryPut "foo" var
  res3 ← AVar.tryTake var
  pure (res1 == Nothing && res2 && res3 == Just "foo")

test_put_take ∷ Effect Unit
test_put_take = test "put/take" do
  ref ← Ref.new ""
  var ← AVar.empty
  _ ← AVar.put "foo" var $ traverse_ \_ →
    void $ Ref.modify (_ <> "bar") ref
  _ ← AVar.take var $ traverse_ \val →
    void $ Ref.modify (_ <> val) ref
  eq "barfoo" <$> Ref.read ref

test_put_read_take ∷ Effect Unit
test_put_read_take = test "put/read/take" do
  ref ← Ref.new ""
  var ← AVar.empty
  _ ← AVar.put "foo" var $ traverse_ \_ →
    void $ Ref.modify (_ <> "bar") ref
  _ ← AVar.read var $ traverse_ \val →
    void $ Ref.modify (_ <> val <> "baz") ref
  _ ← AVar.take var $ traverse_ \val →
    void $ Ref.modify (_ <> val) ref
  eq "foobazfoobar" <$> Ref.read ref

test_take_put ∷ Effect Unit
test_take_put = test "take/put" do
  ref ← Ref.new ""
  var ← AVar.empty
  _ ← AVar.take var $ traverse_ \val →
    void $ Ref.modify (_ <> val) ref
  _ ← AVar.put "foo" var $ traverse_ \_ →
    void $ Ref.modify (_ <> "bar") ref
  eq "foobar" <$> Ref.read ref

test_take_read_put ∷ Effect Unit
test_take_read_put = test "take/read/put" do
  ref ← Ref.new ""
  var ← AVar.empty
  _ ← AVar.take var $ traverse_ \val →
    void $ Ref.modify (_ <> val) ref
  _ ← AVar.read var $ traverse_ \val →
    void $ Ref.modify (_ <> val <> "baz") ref
  _ ← AVar.put "foo" var $ traverse_ \_ →
    void $ Ref.modify (_ <> "bar") ref
  eq "foobazfoobar" <$> Ref.read ref

test_read_put_take ∷ Effect Unit
test_read_put_take = test "read/put/take" do
  ref ← Ref.new ""
  var ← AVar.empty
  _ ← AVar.read var $ traverse_ \val →
    void $ Ref.modify (_ <> val <> "baz") ref
  _ ← AVar.put "foo" var $ traverse_ \_ →
    void $ Ref.modify (_ <> "bar") ref
  _ ← AVar.take var $ traverse_ \val → do
    void $ Ref.modify (_ <> val) ref
  eq "foobazbarfoo" <$> Ref.read ref

test_read_take_put ∷ Effect Unit
test_read_take_put = test "read/take/put" do
  ref ← Ref.new ""
  var ← AVar.empty
  _ ← AVar.read var $ traverse_ \val → do
    void $ Ref.modify (_ <> val <> "baz") ref
    void $ AVar.take var $ traverse_ \val' →
      void $ Ref.modify (_ <> val') ref
  _ ← AVar.put "foo" var $ traverse_ \_ →
    void $ Ref.modify (_ <> "bar") ref
  eq "foobazbarfoo" <$> Ref.read ref

test_kill_full ∷ Effect Unit
test_kill_full = test "kill/full" do
  ref ← Ref.new ""
  var ← AVar.empty
  _ ← AVar.put "foo" var $ traverse_ \_ →
    void $ Ref.modify (_ <> "bar") ref
  AVar.kill (error "Die.") var
  _ ← AVar.read var case _ of
    Left err → void $ Ref.modify (_ <> message err) ref
    Right _  → void $ Ref.modify (_ <> "BAD") ref
  eq "barDie." <$> Ref.read ref

test_kill_empty ∷ Effect Unit
test_kill_empty = test "kill/empty" do
  ref ← Ref.new ""
  var ← AVar.empty
  AVar.kill (error "Die.") var
  _ ← AVar.read var case _ of
    Left err → void $ Ref.modify (_ <> message err) ref
    Right _  → void $ Ref.modify (_ <> "BAD") ref
  eq "Die." <$> Ref.read ref

test_kill_pending ∷ Effect Unit
test_kill_pending = test "kill/pending" do
  ref ← Ref.new ""
  var ← AVar.empty
  let
    cb s = case _ of
      Left err → void $ Ref.modify (_ <> s <> message err) ref
      Right _  → void $ Ref.modify (_ <> "BAD") ref
  _ ← AVar.take var (cb "a")
  _ ← AVar.take var (cb "b")
  _ ← AVar.read var (cb "c")
  _ ← AVar.read var (cb "d")
  AVar.kill (error "-die.") var
  eq "c-die.d-die.a-die.b-die." <$> Ref.read ref

test_cancel ∷ Effect Unit
test_cancel = test "cancel" do
  ref ← Ref.new ""
  v1 ← AVar.new ""
  c1 ← AVar.put "a" v1 $ traverse_ \_ → void $ Ref.modify (_ <> "a") ref
  c2 ← AVar.put "b" v1 $ traverse_ \_ → void $ Ref.modify (_ <> "b") ref
  c3 ← AVar.put "c" v1 $ traverse_ \_ → void $ Ref.modify (_ <> "c") ref
  c1
  c2
  _  ← AVar.tryTake v1
  _  ← AVar.tryTake v1
  _  ← AVar.tryTake v1
  v2 ← AVar.empty
  c4 ← AVar.take v2 $ traverse_ \_ → void $ Ref.modify (_ <> "d") ref
  c5 ← AVar.take v2 $ traverse_ \_ → void $ Ref.modify (_ <> "e") ref
  c6 ← AVar.take v2 $ traverse_ \_ → void $ Ref.modify (_ <> "f") ref
  c5
  _  ← AVar.tryPut "a" v2
  _  ← AVar.tryPut "b" v2
  _  ← AVar.tryPut "c" v2
  v3 ← AVar.empty
  c7 ← AVar.read v3 $ traverse_ \_ → void $ Ref.modify (_ <> "g") ref
  c8 ← AVar.read v3 $ traverse_ \_ → void $ Ref.modify (_ <> "h") ref
  c9 ← AVar.read v3 $ traverse_ \_ → void $ Ref.modify (_ <> "i") ref
  c8
  c9
  _  ← AVar.tryPut "a" v3
  eq "cdfg" <$> Ref.read ref

main ∷ Effect Unit
main = do
  test_tryRead_full
  test_tryRead_empty
  test_tryPut_full
  test_tryPut_empty
  test_tryTake_full
  test_tryTake_empty
  test_put_take
  test_take_put
  test_take_read_put
  test_read_put_take
  test_read_take_put
  test_kill_full
  test_kill_empty
  test_kill_pending
  test_cancel
