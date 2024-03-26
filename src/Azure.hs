module Azure where

import Types 
import EnvVars

-- PUT https://management.azure.com/subscriptions/{subscriptionId}/resourceGroups/{resourceGroupName}/providers/Microsoft.CognitiveServices/accounts/{accountName}/deployments/{deploymentName}?api-version=2023-05-01
-- createDeploymentReq :: 


createDeployment :: CreateDeploymentRequest -> IO (CreateDeploymentRes) 
createDeployment req = do
    accountName <- azureAccountName 
    resourceGroup <- azureResourceGroupName 
    subsId <- azureSubscriptionId 
    apiVersion <- azureAPIVersion

    pure $ CreateDeploymentFailure "Not imlemented" "500"