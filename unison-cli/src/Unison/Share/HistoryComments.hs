module Unison.Share.HistoryComments () where

uploadHistoryCommentsImpl :: Codebase IO v a -> BranchRef -> Connection -> WebApp ()
uploadHistoryCommentsImpl codebase = _

type HistoryCommentsAPI = ("ucm" :> "v1" :> "history-comments" :> HistoryComments.API)

historyCommentsAPI :: Proxy HistoryCommentsAPI
historyCommentsAPI = Proxy @HistoryCommentsAPI

downloadCommentsClientM :: BranchRef -> Connection -> Servant.ClientM ()
uploadCommentsClientM :: BranchRef -> Connection -> Servant.ClientM ()

SyncV2.Routes
  { downloadEntitiesStream = downloadEntitiesStreamClientM,
    causalDependenciesStream = causalDependenciesStreamClientM
  } = Servant.client historyCommentsAPI
