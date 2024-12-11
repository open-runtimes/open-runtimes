import { Routes } from '@angular/router';
import { HomepageComponent } from './homepage/homepage.component';
import { DateComponent } from './date/date.component';

export const routes: Routes = [
    {path: '', component: HomepageComponent},
    {path: 'date', component: DateComponent},
];
