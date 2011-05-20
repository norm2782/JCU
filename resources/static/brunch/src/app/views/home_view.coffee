homeTemplate = require('templates/home')
Rule = require('models/rule_model').Rule

class exports.HomeView extends Backbone.View
  id: 'home-view'

  events:
    'click #btnCheck'   : 'checkRules'
    'click #btnAddRule' : 'addStoreRule'

  render: =>
    @$(@.el).html homeTemplate
    @$('#proof-tree-div').append app.views.proofTree.render()
    @$('#rules-list-div').append app.views.rulesList.render().el
    @

  addStoreRule: =>
    txtAddRule = @$('#txtAddRule')
    txtVal = txtAddRule.val()

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

  # TODO: Rework this to use the new checking system.
  checkRules: =>
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

    # TODO: Defer this to a Model
    # if false
    #   alert "All fields must contain a valid expression before you can check them for correctness."
    # else
    #   $.ajax
    #     url:  '/rules/check'
    #     type: 'POST'
    #     contentType: 'application/json'
    #     dataType: 'json'
    #     data:     JSON.stringify app.models.tree.get('treeRoot')
    #     success:  callback
