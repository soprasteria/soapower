function BulkCtrl($scope, $rootScope, $location, ConfigurationService) {
    $scope.downloadConfigurationURL = "/bulk/downloadConfiguration";
    $scope.uploadConfigurationURL = "/bulk/uploadConfiguration";
    $scope.showResponseUpload = false;
    $scope.showUploadRunning = false;
    $scope.uploadCompleted = function (content) {
        $scope.response = content;
        $scope.showUploadRunning = false;
        $scope.showResponseUpload = true;
    };

    ConfigurationService.configurationExample().
    success(function (data) {
        $scope.exampleCode = data;
    })
    .error(function (resp) {
        console.log("Error with ConfigurationService.configurationExample" + resp);
    });
}