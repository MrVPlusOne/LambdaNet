import { Component, ElementRef, HostListener } from 'mojiito-core';

@Component({
  selector: '[my-button]'
})
export class ButtonComponent {

  constructor(private elementRef: ElementRef) {
    console.log(elementRef.nativeElement);
  }

  @HostListener('click')
  clicked() {
    console.log('Button clicked');
  }

}
