import { Routes } from '@angular/router';
import { HomepageComponent } from './homepage/homepage.component';
import { DateComponent } from './date/date.component';
import { LogsComponent } from './logs/logs.component';
import { ExceptionComponent } from './exception/exception.component';
import { LibraryComponent } from './library/library.component';
import { ConcurrencyComponent } from './concurrency/concurrency.component';

export const routes: Routes = [
    {path: '', component: HomepageComponent},
    {path: 'date', component: DateComponent},
    {path: 'logs', component: LogsComponent},
    {path: 'exception', component: ExceptionComponent},
    {path: 'library', component: LibraryComponent},
    {path: 'concurrency', component: ConcurrencyComponent},
];
