import { ViewData } from './types';

export function attachEmbeddedView(parentView: ViewData, viewIndex: number, view: ViewData) {
  let embeddedViews = parentView.viewContainer._embeddedViews;
  if (viewIndex == null) {
    viewIndex = embeddedViews.length;
  }
  view.viewContainerParent = parentView;
  addToArray(embeddedViews, viewIndex, view);
}

export function detachEmbeddedView(parent: ViewData, viewIndex: number): ViewData {
  const embeddedViews = parent.viewContainer._embeddedViews;
  if (viewIndex == null || viewIndex >= embeddedViews.length) {
    viewIndex = embeddedViews.length - 1;
  }
  if (viewIndex < 0) {
    return null;
  }
  const view = embeddedViews[viewIndex];
  view.viewContainerParent = undefined;
  removeFromArray(embeddedViews, viewIndex);
  return view;
}

function addToArray(arr: any[], index: number, value: any) {
  // perf: array.push is faster than array.splice!
  if (index >= arr.length) {
    arr.push(value);
  } else {
    arr.splice(index, 0, value);
  }
}

function removeFromArray(arr: any[], index: number) {
  // perf: array.pop is faster than array.splice!
  if (index >= arr.length - 1) {
    arr.pop();
  } else {
    arr.splice(index, 1);
  }
}
