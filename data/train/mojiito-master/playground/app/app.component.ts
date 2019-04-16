import { Component, ViewContainerRef, Renderer, Inject } from 'mojiito-core';

import {Dialog, DialogComponent } from './dialog';
import { ButtonComponent } from './button.component';
import { DrawerComponent } from './drawer.component';

@Component({
  selector: 'body',
  components: [ DialogComponent, ButtonComponent, DrawerComponent ],
  providers: [Dialog]
})
export class AppComponent {
  private dialogTemplate: string;
  constructor(private renderer: Renderer, private _dialog: Dialog) {
    const buttonEl = renderer.selectRootElement('#open');

    renderer.listen(buttonEl, 'click', this.openDialog.bind(this));
  }

  openDialog(event) {
    this._dialog.open();
  }
}

// document.addEventListener
