homeTemplate = require('templates/home')


class exports.HomeView extends Backbone.View
  id: 'home-view'

  events:
    'click #btnAdd'   : 'addRule'
    'click #btnCheck' : 'checkRules'
    'click #btnHint'  : 'getHint'

  render: ->
    @$(@.el).html homeTemplate
    @$(@.el).find('#rules-tree-div').append app.views.rulesTree.render().el
    @$(@.el).find('#rules-list-div').append app.views.rulesList.render().el
    @

  addRule: -> alert "addRule"

  checkRules: ->
    callback = (data) ->
      f = (x, xs) -> x && xs
      if _.foldr data, f, true
        $("#dialog").html("That's correct!")
        $("#dialog").dialog
          title: "That's correct!",
          modal: true
          buttons:
            Ok: => $("#dialog").dialog("close")
      else
        $('#rules-tree-div input[type="text"]').each(
          -> if data.shift()
               $(this).css "background-color", "#afa"
             else
               $(this).css "background-color", "#faa"
        )

    $.ajax
      url:  '/rules/check'
      type: 'POST'
      contentType: 'application/json'
      dataType: 'json'
      data:     JSON.stringify app.collections.rulesTree
      success:  callback

  getHint: -> alert "getHint"

