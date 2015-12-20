Как загрузить:
1.
M-x slime-cd
Выбрать директорию с файлом ccs.asd

2. 
(asdf:operate 'asdf:load-op :ccs)
(asdf:operate 'asdf:load-op :ccs-test)


Запуск тестов:
(lift:run-tests :suite 'root :break-on-errors? nil)
