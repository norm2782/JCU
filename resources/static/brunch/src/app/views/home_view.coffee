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

  addRule: ->
    app.collections.rulesTree.add {}

  checkRules: ->
    # TODO: Clicking check right after add doesn't color the added text field
    # Do we really want all of this here? Or do we want to delegate parts of
    # it all to the individual models?
    callback = (data) ->
      flds = $('#rules-tree-div input[type="text"]')
      if _.and data
        alert "That's correct!"
        flds.each( -> $(this).css "background-color", "#fff")
      else
        flds.each(
          -> if data.shift()
               $(this).css "background-color", "#afa"
             else
               $(this).css "background-color", "#faa"
        )

    invalids = app.collections.rulesTree.any(
      (x) ->
        rule = x.get "rule"
        !rule? || !(x.validate rule)
    )

    if invalids
      alert "All fields must contain a valid expression before you can check them for correctness."
    else
      $.ajax
        url:  '/rules/check'
        type: 'POST'
        contentType: 'application/json'
        dataType: 'json'
        data:     JSON.stringify app.collections.rulesTree
        success:  callback

  getHint: -> alert "getHint"

