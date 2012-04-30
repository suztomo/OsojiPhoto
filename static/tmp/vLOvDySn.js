$(document).ready(function() {
$.ajax({
  url: "http://localhost:3000/messages/all.json",
  dataType: 'json',
  success: onMessageLoad
});
})
function onMessageLoad(data) {
    var i;
    k = data;
    var messages = data.messages;
    var images = data.images;
    var messageToImage = {};
    userIdToUser = {};
    var users = data.users;
    for (i=0; i<users.length; ++i) {
        var user = users[i];
        userIdToUser[user.id] = user;
    }
    for (i=0; i<images.length; ++i) {
        var img = images[i];
        if (typeof messageToImage[img.postId] == "undefined") {
            messageToImage[img.postId] = [];
        }
        messageToImage[img.postId].push(img);
    }
    for (i=0; i<messages.length; ++i) {
        var m = messages[i];
        var d = $("<div class='message-box' />");
        var imgList = messageToImage[m.id];
        if (typeof imgList == 'undefined') {
            continue;
        }
        for (j=0; j<imgList.length; j++) {
            var img = imgList[j]; // use the first one 
            d.append($("<p class='osoji-photo'><a href='"+m.googleLink+"'><img height='100' src='"+ img.imageURL +"' /></a></p>"));
            break;
        }
        d.append($("<p />").text(m.message));
        d.append($('<g:plusone size="small" annotation="inline" href="'+ m.link
                   +'"></g:plusone>'));
        var u = userIdToUser[m.userId];
        if (typeof u != "undefined") {
            var userName = $("<a href='"+u.link+"' class='user-name' />").text(u.name);
            d.append(userName);
        }
        $("#messages").append(d);
    }
}


$((function(){
    $.get("http://localhost:3000/profile/self.json", null, function(data){
          if (data.error != null) {
              showPermanentUserInfoForGuest()
          } else {
              showPermanentUserInfoForUser(data);
              $('body').trigger('login', [data]);
          }
    });
    $('body').bind('login', function(ev, userInfo) {
        $.get("http://localhost:3000/news/num/5/page/0", null, function(data) {
            var news = data.news;
            if (news == null) {
                return;
            }
            var i;
            var newsListElem = $("#news-notification-list");
            newsListElem.empty();
            for (i=0; i<news.length; ++i) {
                var item = news[i];
                var itemElem = $("<a style='display:block' />").text(item.message).attr("href", item.url);
                var d = new Date(item.createdAt);
                var dateElem = $("<span class='date'/>").text(
                    d.getHours() +":"+d.getMinutes());
                itemElem.append(dateElem);
                var li = $("<li />").append(itemElem);
                newsListElem.append(li);
            }
            $("#news-notification").show();
        });
    });
    $('.googlePlusOAuth .googlePlusOAuthLogin').addClass('btn btn-primary');
    if (
})());

function resizeImageURL(url, height) {
    return "http://images0-focus-opensocial.googleusercontent.com/gadgets/proxy?container=focus&gadget=a&resize_h="+height+"&url=" + encodeURIComponent(url)
}

function showPermanentUserInfoForUser(userInfo) {
    var resizedURL = resizeImageURL(userInfo.pictureURL, 25);
    $("#login-user-icon").css('background-image', 'url('+resizedURL+')');
    $("#login-user-name").text(userInfo.name);
    $("#for-login-user").fadeIn();

}
function showPermanentUserInfoForGuest(){
    $("#for-guest").fadeIn();
}

