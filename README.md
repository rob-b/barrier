# barrier

## For testing:

```
import Data.Maybe (listToMaybe)
(Just config) <- mkAppConfig
payload <- payloadFromFixture
let wrappedHook = WrappedHookPullRequest payload
getStoryLinkFromPayload payload
links = maybeToList $ getStoryLinkFromPayload payload
setPullRequestStatus links payload config

```



```
import GitHub.Data.Webhooks.Events
import GitHub.Data.Webhooks.Payload
comment <- issueCommentEventFromFixture
(Just xo) = evIssueCommentPayload <$> comment
whUserLogin $ whIssueCommentUser xo
(Just wrappedHook) = (getWrappedHookFromIssue =<< comment)
```


```
(Just issueCommentEvent) <- issueCommentEventFromFixture
(Just hookIssueComment) = getIssueFromEvent issueCommentEvent
(Just config) <- mkAppConfig
doThingForComment hookIssueComment config
```
