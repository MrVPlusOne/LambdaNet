import { Component, ElementRef, ChildListener } from 'mojiito-core';

@Component({
  selector: 'side-drawer'
})
export class DrawerComponent {

  constructor(private elementRef: ElementRef) {
    console.log(elementRef.nativeElement);
  }

  @ChildListener('button', 'click')
  childClicked() {
    console.log('Child clicked');
  }

}
