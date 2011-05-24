homeTemplate = require('templates/home')
Rule = require('models/rule_model').Rule

class exports.HomeView extends Backbone.View
  id: 'home-view'

  events:
    'click #btnCheck'       : 'checkProof'
    'click #btnAddRule'     : 'addStoreRule'
    'click #btnReset'       : 'resetTree'
    'keypress #txtAddRule'  : 'addEnterRule'
    "blur #txtAddRule"      : "checkRuleSyntax"

  initialize: =>
    @valid = false

  render: =>
    @$(@.el).html homeTemplate
    @$('#proof-tree-div').append app.views.proofTree.render()
    @$('#rules-list-div').append app.views.rulesList.render().el
    @

  addEnterRule: (evt) =>
    @addStoreRule() if evt.which == 13

  setBgColor: (fld, cls) =>
    fld.removeClass 'redField yellowField greenField whiteField blueField'
    fld.addClass cls

  checkRuleSyntax: =>
    txtAddRule = @$('#txtAddRule')

    view = @
    callback = (data) ->
       # TODO: Error message
      console.log data
      # data[0] : Boolean indicating whether we have a successful parse or not
      # data[1] : List of error strings indicating what went wroning during parsing
      if data[0]
        bgc = "whiteField"
        view.valid = true
      else
        bgc = "blueField"
        view.valid = false
      view.setBgColor txtAddRule, bgc

    $.ajax
      type:  'POST'
      url:   "/check-syntax/rule"
      data:  txtAddRule.val()
      contentType: 'application/json'
      dataType: 'json'
      success:  callback

  resetTree: =>
    app.models.tree.reset()

  addStoreRule: =>
    @checkRuleSyntax()
    val = @$('#txtAddRule').val()

    if @valid
      res = app.collections.rulesList.find(
        (x) ->
          rl = x.get("rule").replace(/\s+/g, '')
          vl = val.replace(/\s+/g, '')
          return rl == vl
      )

      if !res?
        app.collections.rulesList.create(new Rule({rule: val}))
      @$('#txtAddRule').val("")

  checkProof: =>
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
