# barrier

## For testing:

```
import Data.Maybe (listToMaybe)
(Just config) <- mkAppConfig
payload <- payloadFromFixture
let wrappedHook = WrappedHookPullRequest payload
getStoryLinkFromHook wrappedHook
links = maybeToList $ getStoryLinkFromHook wrappedHook
setPullRequestStatus links payload config
```
