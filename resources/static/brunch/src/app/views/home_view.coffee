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
      flds = $('#rules-tree-div input[type="text"]')
      if _.all data, _.identity
        dial = $("#dialog")
        dial.html("That's correct!")
        dial.dialog
          title: "That's correct!",
          modal: true
          buttons:
            Ok: => dial.dialog("close")
        flds.each( -> $(this).css "background-color", "#fff")
      else
        flds.each(
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

