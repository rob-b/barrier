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
