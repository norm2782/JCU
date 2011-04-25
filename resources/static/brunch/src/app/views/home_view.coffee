homeTemplate = require('templates/home')
Rule = require('models/rule_model').Rule

class exports.HomeView extends Backbone.View
  id: 'home-view'

  events:
    'click #btnAdd'     : 'addTreeRule'
    'click #btnCheck'   : 'checkRules'
    'click #btnHint'    : 'getHint'
    'click #btnAddRule' : 'addStoreRule'

  render: ->
    @$(@.el).html homeTemplate
    @$('#rules-tree-div').append app.views.rulesTree.render().el
    @$('#rules-list-div').append app.views.rulesList.render().el
    @

  addTreeRule: ->
    app.collections.rulesTree.add {}

  addStoreRule: ->
    txtAddRule = @$('#txtAddRule')
    txtVal = txtAddRule.val()

    # TODO: Make this actually work
    newRule = new Rule({id: "", rule: txtVal})

    if newRule.validate()
      res = app.collections.rulesList.find(
        (x) ->
          r = (x.get "rule")
          r == txtVal
      )

      if !res?
        app.collections.rulesList.create newRule
      color = "#fff"
    else
      color = "#faa"

    txtAddRule.css "background-color", color

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
      (rule) -> !rule? || !x.validate
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

