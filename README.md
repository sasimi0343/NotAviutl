# AviutlScripts
AviUtl�ō���Ă����X�N���v�g��"�ȑO�̂��茵�I����"���܂����B
�Ȃ�ׂ����삪���肷����̂��������Ă��܂��B

## ���������ӓ_
patch.aul�����rikky_module (var1�����ł����Ǝv�����Ǘ�������Ă����ƈ��S) ���K�v�ł��B

# �e�X�N���v�g
## ExtremeTransion
�ʃI�u�W�F�N�g�̓o��p�ɍ��܂����B
�A�j���[�V�������ʂ̖��[��"@ET"�����Ă�����̂����̃X�N���v�g�ł��B

### ���ʃp�����[�^
[Global] ���擪�ɂ��Ă�����̂�ExtremeTransion�����ʂ̃p�����[�^�ł�
* [Global]Delay
  * �ʃI�u�W�F�N�g�̔ԍ����ɂ��炵�ēo�ꂳ���鎞�̒x��(�t���[����)�ł��B
  * �Ⴆ��Delay��2�̏ꍇ�A�ԍ�1�̃I�u�W�F�N�g��2�t���[����ɔԍ�2�̃I�u�W�F�N�g���o�ꂵ�܂��B
* [Global]Time
  * �o��ɂ����鎞��(�t���[����)�ł��B
  * �C�[�W���O�ɂ����鎞�Ԃɉe�����܂��B
* [Global]GlobalDelay
  * �S�̂ɂ�����x��(�t���[����)�ł��B
* Easing
  * �C�[�W���O�̔ԍ��ł��B **���̐��ł����Curve Editor�̔ԍ�** �A **���̐��ł���΃C�[�W���O�̔ԍ�** �ɂȂ�܂��B

(����: �uMove@ET�v��Start���W�����]���Ă��܂��B)
(����: �uFanClipping@ET�v�̎g�p�ɂ�rikky����́u��N���b�s���O�v�̃X�N���v�g����u"fan_clipping.lua"�v��ExtremeTransion�̃t�H���_�ɓ����K�v������܂��B)

### �ǋL (2024/03/14)
�e�A�j���[�V�������ʂɁuExit�v�̃`�F�b�N��ǉ����܂����B
�o�ꂩ��ޏo�ɂ���Ƃ��Ƀ`�F�b�N�{�b�N�X�Ƀ`�F�b�N�����Ă��������B

## MoreShapes
���܂��܂Ȑ}�`��I�u�W�F�N�g��ǉ����邽�߂ɍ��܂����B
�܂��AMoreShapes3D�ł́A3D�I�u�W�F�N�g�̕`�悪�ł���悤�ɂ��Ă��܂��B
�}�`�́u�J�X�^���I�u�W�F�N�g�v�ɓ����Ă��܂��B

(��r�I����͕s����ȕ��ł�)

### MoreShapes3D�̎g���� (��{)
1. �e�N�X�`���ƂȂ郁�f�B�A�I�u�W�F�N�g��ǉ����܂��B(�l�p�`��摜�Ȃ�)
2. �I�u�W�F�N�g���g���`��ɂ��A�J����������ɓ���܂��B
3. �A�j���[�V�������ʁu[3DShape]Box�v��ǉ����܂��B(�����̂ɂȂ�܂��B)
   * ���Ȃ�u[3DShape]Ball�v
   * n�p���Ȃ�u[3DShape]Corn�v
   * �����̂́u[3DShape]BoxEx�v
4. �K�v�ɉ����Ēǉ��Ō��ʂ������܂��B
5. �Ō�Ɂu[3D]Render�v�������܂��B

(�ꕔ�@�\�̎g�p�ɂ́uGetColor�v�́ugetcolortools.lua�v��MoreShapes�t�H���_���ɓ����K�v������܂��B)

## NotAviutl
�l�X�ȃG�t�F�N�g�A���G�ȃJ�X�^���I�u�W�F�N�g��ǉ����܂��B
�A�j���[�V�������ʁA�J�X�^���I�u�W�F�N�g�̖��O�̐ړ����ɂ���Ď�ނ�����܂��B
(�A�j���[�V�������ʂ�"anm"�A�J�X�^���I�u�W�F�N�g��"obj"�Ƃ��܂��B)

| �ړ��� | ��� | ���� |
| ------ | ---- | ---- |
| [Camera] | obj/cam | ���g�p�񐄏��A�J�����̈ʒu�𕔕��I�ɕς��悤�Ƃ��܂����B |
| [Effect] | anm | �G�t�F�N�g�S�ʁA�ꕔ����s����Ȃ��̂�����܂��B |
| [Glitch] | obj | �O���b�`�n�������܂��B���ނ̂݁B |
| [Group] | anm/obj | �O���[�v�������܂��B **�g�p���@�͌�q���܂��B** |
| [Liquid] | anm | �t�̕��̃G�t�F�N�g�������܂��B���ނ̂݁B |
| [Motion] | anm | �����ɍ��킹���G�t�F�N�g�������܂��B���ނ̂݁B |
| [Path] | anm | ���̃\�t�g�ɂ��肪����"�p�X"�Ƃ����T�O��ǉ����܂��B |
| Pen | anm | �y���ł��B |
| [Screen] | obj | ��ʑS�̂ɂ�����G�t�F�N�g��ǉ����܂��B |
| [Text] | anm | �e�L�X�g�p�ɍ��܂����B(ControlPad�̎g�p�͔񐄏�) |
| [Utility] | anm | ��������֗����ȂƂ����m���ō��܂����B |
| [Value] | obj | �g���b�N�o�[�ŕϐ��𑀍삵�܂��B�X�N���v�g�����A�A�j���[�V�������ʂ̃p�����[�^�ݒ�ȂǂŎg�p����p�ł��B |

### �O���[�v���䍇���̕��@ (�ł����肳�Ɨe�Ղ����d��)
1. �����������I�u�W�F�N�g�̏�̃��C���[�ɃJ�X�^���I�u�W�F�N�g�u[Group] InitializeGroup�v��ǉ����܂��B
2. �����������I�u�W�F�N�g�ɃO���[�v�����ǉ����܂��B
3. �O���[�v����ɃA�j���[�V�������ʁu[Utility] MakeGroup�v��ǉ����܂��B
4. �����������I�u�W�F�N�g�� **��** �̃��C���[�ɃJ�X�^���I�u�W�F�N�g�u[Group] ShowGroup�v��ǉ����܂��B

([Group]���Ɏ����悤�Ȃ��̂�����܂����A�قƂ�ǉ��ʌ݊��Ȃ̂ŁA�g�p�͂������߂��܂���B)

