//goog.provide('litenote.Watcher');
const litenote = {};

/**
 * @param {Object} elmApp
 * @constructor
 */
litenote.Watcher = function(elmApp) {
  /**
   * @private
   * @number
   */
  this.lastParentId_ = null;

  /**
   * @private
   * @type {number}
   */
  this.lastInsertPos_ = null;

  /**
   * @private
   */
  this.elmApp_ = elmApp;

  document.body.addEventListener('mousemove',
                          this.handleMouseMove_.bind(this));
};

/**
 * @const {string}
 */
litenote.Watcher.BLOCK_ID_ATTR_NAME = 'data-block-id';

/**
 * @param {Event} e
 * @private
 */
litenote.Watcher.prototype.handleMouseMove_ = function (e) {
  const isPlaceholder = e.target.classList.contains(litenote.Watcher.Classes.PLACEHOLDER) ||
        e.target.parentNode.classList.contains(litenote.Watcher.Classes.PLACEHOLDER);
  if (isPlaceholder) {
    return;
  }

  const rootEl = litenote.Watcher.getBlockRoot_(e.target);

  let newBlockId = null;

  if (!(rootEl instanceof Element)) return;

  newBlockId = parseInt(rootEl.getAttribute(litenote.Watcher.BLOCK_ID_ATTR_NAME), 10);

  if (litenote.Watcher.isContainer_(rootEl)) {
    this.elmApp_.ports.blockId.send({
      parentId: newBlockId,
      insertPos: 0
    });
    return;
  }

  const parentRootEl = litenote.Watcher.getBlockRoot_(rootEl.parentNode);

  if (!(parentRootEl instanceof Element)) return;

  const id = parseInt(parentRootEl.getAttribute(litenote.Watcher.BLOCK_ID_ATTR_NAME), 10);

  const direction = parentRootEl.getAttribute('data-direction');

  let center = 0;
  let current = 0;

  let rect = rootEl.getBoundingClientRect();

  if ('row' === direction) {
    let min = rect.left;
    let max = rect.left + rect.width;
    center = (min + max) / 2;
    current = e.clientX;
  }
  else if ('column' === direction) {
    let min = rect.top;
    let max = rect.top + rect.height;
    center = (min + max) / 2;
    current = e.clientY;
  }
  else {
    throw new Error(`invalid direction ${direction}`);
  }

  const currentIndex = litenote.Watcher.getCellIndex_(rootEl);

  const insertBefore = current < center;

  const insertPos = insertBefore ? currentIndex : currentIndex + 1;

  if (this.lastParentId_ !== id || this.lastInsertPos_ !== insertPos) {
    this.elmApp_.ports.blockId.send({
      parentId: id,
      insertPos: insertPos
    });
    
    this.lastParentId_ = id;
    this.lastInsertPos_ = insertPos;
  }
};


/**
 * @param {Element} node
 * @return {number}
 * @private
 */
litenote.Watcher.getCellIndex_ = function(node) {
  let index = 0;
  while (null !== (node = node.previousElementSibling)) {
    const isBlock = node.classList.contains('grid__cell');
    if (isBlock) {
      index++;
    }
  }
  return index;
};

/**
 * @param {Element} el
 * @return {Node?}
 * @private
 */
litenote.Watcher.getBlockRoot_ = function(el) {
    let current = el;
    while (current !== document.body) {
        if (current.classList.contains('block')) {
            return current;
        }
        current = current.parentNode;
    }
    return null;
};

/**
 * @param {Element} el
 * @return {bool}
 * @private
 */
litenote.Watcher.isContainer_ = function(el) {
  return el.classList.contains('grid__cell--row') ||
    el.classList.contains('grid__cell--column');
};


/**
 * @enum {string}
 */
litenote.Watcher.Classes = {
  PLACEHOLDER: 'placeholder'
};
