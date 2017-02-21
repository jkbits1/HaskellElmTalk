import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { RxItemComponent } from './rx-item.component';

describe('RxItemComponent', () => {
  let component: RxItemComponent;
  let fixture: ComponentFixture<RxItemComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ RxItemComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RxItemComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
