class exports.ProofTreeNode extends Backbone.Model
  # Available attributes:
  # term :: Term
  # childTerms :: BackBone.Collection

  initialize: =>
    @set {childTerms: new Backbone.Collection()}

  hasTerm: =>
    @get('term')?

  getChildTerms: =>
    @get('childTerms')

  setChildNo: (childNo) =>
    newNo = childNo - @getChildTerms().length
    @addRule(false) for i in [1..newNo] if newNo > 0

  addRule: (change) =>
    @getChildTerms().add(new ProofTreeNode())
    @change() if change

  isValid: =>
    return true # TODO: Remove
    if !@hasTerm()
      return false
    # TODO: Make sure a Rule object is available here

    @get('term').validate() && @getChildTerms().all((x) -> x.isValid())

  clear: =>
    @destroy()

