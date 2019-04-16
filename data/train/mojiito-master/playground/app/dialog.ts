import {
  Component, ElementRef, Injectable, Inject, Injector,
  ComponentFactoryResolver, ComponentRef, Renderer
} from 'mojiito-core';
import { DOCUMENT } from 'mojiito-platform-browser';

@Component({
  selector: '.mdl-dialog__actions'
})
export class DialogActionsComponent {

  constructor(elementRef: ElementRef, renderer: Renderer, dialog: Dialog) {
    const buttons = [].slice.call(elementRef.nativeElement.querySelectorAll('button'));
    buttons.forEach(b => renderer.listen(b, 'click', dialog.close.bind(dialog)));
  }

}

@Component({
  selector: 'dialog',
  components: [DialogActionsComponent]
})
export class DialogComponent {

  constructor(elementRef: ElementRef, renderer: Renderer) {
    renderer.setAttribute(elementRef.nativeElement, 'open', '');
  }

}


@Injectable()
export class Dialog {

  private _dialogTemplate: string;
  private _openDialog: ComponentRef<DialogComponent>;

  constructor( @Inject(DOCUMENT) private _doc: Document, private _inj: Injector,
    private _resolver: ComponentFactoryResolver) {
    const templateEl = _doc.getElementById('dialog-template');
    this._dialogTemplate = templateEl.innerHTML;
  }

  get isOpen() { return !!this._openDialog; }

  open() {
    if (this.isOpen) {
      throw new Error('There is already a dialog open.');
    }

    // create and append element
    const el = this._createDialogElement();
    this._doc.body.appendChild(el);

    const factory = this._resolver.resolveComponentFactory(DialogComponent);
    const ref = factory.create(this._inj, el);
    this._openDialog = ref;
  }

  close() {
    if (!this.isOpen) {
      throw new Error('No dialog open for closing');
    }
    this._openDialog.destroy();
    this._openDialog = null;
  }

  private _createDialogElement() {
    const tmp = this._doc.createElement('div');
    tmp.innerHTML = this._dialogTemplate;
    return tmp.firstElementChild;
  }
}
