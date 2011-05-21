homeTemplate = require('templates/home')
Rule = require('models/rule_model').Rule

class exports.HomeView extends Backbone.View
  id: 'home-view'

  events:
    'click #btnCheck'       : 'checkProof'
    'click #btnAddRule'     : 'addStoreRule'
    'keypress #txtAddRule'  : 'addEnterRule'

  render: =>
    @$(@.el).html homeTemplate
    @$('#proof-tree-div').append app.views.proofTree.render()
    @$('#rules-list-div').append app.views.rulesList.render().el
    @

  addEnterRule: (evt) =>
    @addStoreRule() if evt.which == 13

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
    txtAddRule.val("")

  # TODO: Rework this to use the new checking system.
  checkProof: =>
    # TODO: Clicking check right after add doesn't color the added text field
    # Do we really want all of this here? Or do we want to delegate parts of
    # it all to the individual models?
    callback = (data) ->
      app.models.tree.setProofResult(data)

    if app.models.tree.isValid()
      $.ajax
        url:  '/proof/check'
        type: 'POST'
        contentType: 'application/json'
        dataType: 'json'
        data:     JSON.stringify app.models.tree.treeRoot()
        success:  callback
    else
      alert "Cannot check proof. You have one or more invalid rules in your tree."

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
