import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Message
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Message decoding" $ do
    it "can decode JSON message with InitMessageType" $ do
      let jsonString = "{\"id\":0,\"src\":\"c0\",\"dest\":\"n0\",\"body\":{\"type\":\"init\",\"node_id\":\"n3\",\"node_ids\":[\"n1\", \"n2\", \"n3\"],\"msg_id\":1}}"
      let expected =
            Just
              ( Message
                  { src = "c0",
                    dest = "n0",
                    body =
                      MessageBody
                        { msgType = InitMessageType,
                          msgId = Just 1,
                          inReplyTo = Nothing,
                          code = Nothing,
                          text = Nothing,
                          nodeId = Just "n3",
                          nodeIds = Just ["n1", "n2", "n3"]
                        }
                  }
              )
      decode (BS.pack jsonString) `shouldBe` expected
