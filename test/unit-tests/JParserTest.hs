module JParserTest (jParserTests) where

import Jq.JParser (parseJSON)
import Jq.Json (JSON (..))
import Parsing.Parsing (parse)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Prelude hiding (fail)

jParserTests :: TestTree
jParserTests = testGroup"JParser tests"[
    testCase "nullTest" $ "null" `parseTo` JNull,
    testCase "doubleTest" $ "-0.15" `parseTo` JFloat 0.15,
    testCase "num" $ "200" `parseTo` JFloat 200,
    testCase "trueTest" $ "true" `parseTo` JBool True,
    testCase "hello string" $ "Hello, \"world\"!" `parseTo` JString "Hello, world!",
    testCase "stringTest" $ "ash!" `parseTo` JString "ash!",
    testCase "arrayTest" $ "[1,2,3,4]" `parseTo` JArray [JNum 1, JNum 2,JNum 3,JNum 4],
    testCase "failure" $ fail "tnull"]

parseTo :: String -> JSON -> Assertion
parseTo s j = case parse parseJSON s of
    [(v, "")] -> assertEqual ("Expected:\n" ++ show j ++ "\ngot:\n" ++ show v) j v
    [(v, s)] -> assertFailure $ "Parsed:\n" ++ show v ++ "\nwith remainder:\n" ++ show s
    _ -> assertFailure "Parsing failed"

fail :: String -> Assertion
fail s = case parse parseJSON s of
    [(v, "")] -> assertFailure $ "Parsing should fail but succeeded with:\n" ++ show v
    _ -> return ()
