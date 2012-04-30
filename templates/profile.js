(function() {
  var fillProfile, loadUserMessages, showPermanentUserInfoForGuest;

  fillProfile = function(userInfo) {
    $(".profile-name").text(userInfo.name);
    $(".profile-pictureURL").attr("src", userInfo.pictureURL);
    return $(".profile-link").attr("href", userInfo.link);
  };

  loadUserMessages = function(googleId) {
    var _this = this;
    return $.get("/user/" + googleId + "/messages.json", null, function(data) {
      if (data.error != null) {
        return alert("error");
      } else {
        return onMessageLoad(data);
      }
    });
  };

  showPermanentUserInfoForGuest = function() {
    return $("#for-guest").fadeIn();
  };

  jQuery(function() {
    var _this = this;
    return $.get("@{SelfProfileR}", null, function(data) {
      if (data.error != null) {
        return alert("error");
      } else {
        return fillProfile(data);
      }
    });
  });

}).call(this);
